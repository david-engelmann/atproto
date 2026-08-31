open Cid
open Car
open Dag_cbor
open Mst
open Firehose
open Sync
open Tid

(** Library-shaped TAP helpers: backfill a repo CAR, walk records, apply
    firehose ops, and re-sync when the commit chain breaks.

    This is not a hosted Tap service. Spec:
    https://atproto.com/specs/sync#record-level-synchronization
    https://atproto.com/blog/introducing-tap *)
module Repo_sync = struct
  exception Error of string

  let fail msg = raise (Error msg)

  type status = Desynchronized | In_progress | Synchronized

  type snapshot = {
    did : string;
    rev : string;
    commit_cid : Cid.t;
    data : Cid.t;
    commit : Mst.repo_commit;
    car : Car.t;
    tree : Mst.tree;
  }

  type record_change =
    | Created of { path : string; cid : Cid.t; live : bool }
    | Updated of { path : string; cid : Cid.t; prev : Cid.t; live : bool }
    | Deleted of { path : string; prev : Cid.t; live : bool }

  type account = {
    did : string;
    mutable rev : string;
    mutable data : Cid.t option;
    mutable commit : Cid.t option;
    mutable status : status;
    records : (string, Cid.t) Hashtbl.t;
    pending : Firehose.commit Queue.t;
    collections : string list;
  }

  let starts_with s prefix =
    let n = String.length prefix in
    String.length s >= n && String.sub s 0 n = prefix

  let path_in_scope collections path =
    collections = []
    || List.exists (fun c -> path = c || starts_with path (c ^ "/")) collections

  let split_path path =
    match String.index_opt path '/' with
    | None -> (path, "")
    | Some i ->
        ( String.sub path 0 i,
          String.sub path (i + 1) (String.length path - i - 1) )

  let open_car (car : Car.t) : snapshot =
    let commit_cid =
      match Car.root car with Some c -> c | None -> fail "CAR has no root"
    in
    let block =
      match Car.find_block car commit_cid with
      | Some b -> b
      | None -> fail "commit block missing from CAR"
    in
    let computed = Cid.create block.data in
    if not (Cid.equal computed commit_cid) then
      fail
        (Printf.sprintf "commit CID mismatch: root %s block %s"
           (Cid.to_string commit_cid) (Cid.to_string computed));
    let commit = Mst.parse_repo_commit (Dag_cbor.decode block.data) in
    if commit.version <> 3 then
      fail (Printf.sprintf "unsupported repo commit version %d" commit.version);
    if not (Tid.is_valid commit.rev) then
      fail ("commit rev is not a TID: " ^ commit.rev);
    let store = Mst.store_of_car car in
    Mst.verify_tree ~get_block:(Mst.store_get store) commit.data;
    let tree = Mst.tree_of_root store commit.data in
    {
      did = commit.did;
      rev = commit.rev;
      commit_cid;
      data = commit.data;
      commit;
      car;
      tree;
    }

  let open_car_bytes (bytes : string) : snapshot = open_car (Car.parse bytes)

  let verify_snapshot ?(keys : string list option) (snap : snapshot) : unit =
    Mst.verify_tree
      ~get_block:(Mst.store_get (Mst.store_of_car snap.car))
      snap.data;
    match keys with
    | None -> ()
    | Some ks -> (
        match Mst.verify_commit_sig ~keys:ks snap.commit with
        | `Valid -> ()
        | `Missing -> fail "commit is unsigned"
        | `Invalid -> fail "commit signature is invalid"
        | `Unsupported_curve c -> fail ("commit uses unsupported curve " ^ c))

  let walk (snap : snapshot) : (string * Cid.t) list = Mst.walk snap.tree

  let record_block (car : Car.t) (cid : Cid.t) : string =
    match Car.find_block car cid with
    | None -> fail ("record block missing " ^ Cid.to_string cid)
    | Some b ->
        ignore (Cid.verify_block ~expected:cid ~codec:cid.codec b.data);
        b.data

  let verify_record_proof ~(car : Car.t) ~(path : string) : Cid.t * string =
    let snap = open_car car in
    match Mst.get snap.tree path with
    | None -> fail ("record path not in MST: " ^ path)
    | Some cid -> (cid, record_block car cid)

  let create_account ?(collections = []) ~did () : account =
    if not (Syntax.Syntax.is_valid_did did) then fail ("invalid did " ^ did);
    {
      did;
      rev = "";
      data = None;
      commit = None;
      status = Desynchronized;
      records = Hashtbl.create 64;
      pending = Queue.create ();
      collections;
    }

  let apply_record_op ~live (acct : account) (op : Mst.record_op) :
      record_change option =
    if not (path_in_scope acct.collections op.path) then None
    else
      match op.action with
      | "create" -> (
          match op.cid with
          | None -> fail "create op missing cid"
          | Some cid ->
              Hashtbl.replace acct.records op.path cid;
              Some (Created { path = op.path; cid; live }))
      | "update" -> (
          match op.cid with
          | None -> fail "update op missing cid"
          | Some cid -> (
              match Hashtbl.find_opt acct.records op.path with
              | Some prev ->
                  Hashtbl.replace acct.records op.path cid;
                  Some (Updated { path = op.path; cid; prev; live })
              | None ->
                  Hashtbl.replace acct.records op.path cid;
                  Some (Created { path = op.path; cid; live })))
      | "delete" -> (
          match Hashtbl.find_opt acct.records op.path with
          | Some prev ->
              Hashtbl.remove acct.records op.path;
              Some (Deleted { path = op.path; prev; live })
          | None -> None)
      | other -> fail ("unknown record op " ^ other)

  let apply_record_ops ~live (acct : account) (ops : Mst.record_op list) :
      record_change list =
    List.filter_map (apply_record_op ~live acct) ops

  let diff_walk ~live (acct : account) (walked : (string * Cid.t) list) :
      record_change list =
    let seen = Hashtbl.create (List.length walked) in
    let events = ref [] in
    List.iter
      (fun (path, cid) ->
        if path_in_scope acct.collections path then (
          Hashtbl.add seen path ();
          match Hashtbl.find_opt acct.records path with
          | None ->
              Hashtbl.replace acct.records path cid;
              events := Created { path; cid; live } :: !events
          | Some old when Cid.equal old cid -> ()
          | Some old ->
              Hashtbl.replace acct.records path cid;
              events := Updated { path; cid; prev = old; live } :: !events))
      walked;
    let deletes = ref [] in
    Hashtbl.iter
      (fun path old ->
        if path_in_scope acct.collections path && not (Hashtbl.mem seen path)
        then deletes := (path, old) :: !deletes)
      acct.records;
    List.iter
      (fun (path, prev) ->
        Hashtbl.remove acct.records path;
        events := Deleted { path; prev; live } :: !events)
      !deletes;
    List.rev !events

  let verify_commit_object (c : Firehose.commit) : Mst.repo_commit =
    let signed = Firehose.repo_commit_of c in
    if c.repo <> signed.did then
      fail (Printf.sprintf "commit.did %s != repo %s" signed.did c.repo);
    if c.rev <> signed.rev then
      fail (Printf.sprintf "commit.rev %s != rev %s" signed.rev c.rev);
    signed

  let verify_commit_sig ~keys (c : Firehose.commit) : unit =
    let signed = Firehose.repo_commit_of c in
    match Mst.verify_commit_sig ~keys signed with
    | `Valid -> ()
    | `Missing -> fail "firehose commit is unsigned"
    | `Invalid -> fail "firehose commit signature is invalid"
    | `Unsupported_curve curve ->
        fail ("firehose commit uses unsupported curve " ^ curve)

  let apply_commit_tree (prev : snapshot) (c : Firehose.commit) : snapshot =
    if prev.did <> c.repo then
      fail
        (Printf.sprintf "apply_commit_tree: snapshot %s != repo %s" prev.did
           c.repo);
    let signed = verify_commit_object c in
    Firehose.verify_commit c;
    (match c.prev_data with
    | Some expected when not (Cid.equal expected prev.data) ->
        fail "apply_commit_tree: prevData does not match local MST root"
    | _ -> ());
    let tree = Mst.apply_ops prev.tree (Firehose.record_ops c.ops) in
    let root = Mst.root_cid tree in
    if not (Cid.equal root signed.data) then
      fail
        (Printf.sprintf "applied MST root %s != commit.data %s"
           (Cid.to_string root)
           (Cid.to_string signed.data));
    {
      did = signed.did;
      rev = signed.rev;
      commit_cid = c.commit;
      data = signed.data;
      commit = signed;
      car = c.blocks;
      tree;
    }

  let process_commit ?keys ?(live = true) (acct : account) (c : Firehose.commit)
      : record_change list =
    if c.repo <> acct.did then
      fail (Printf.sprintf "commit repo %s != account %s" c.repo acct.did);
    match acct.status with
    | Desynchronized -> []
    | In_progress ->
        Queue.add c acct.pending;
        []
    | Synchronized ->
        if acct.rev <> "" && String.compare c.rev acct.rev <= 0 then []
        else (
          (match keys with
          | Some ks -> verify_commit_sig ~keys:ks c
          | None -> ());
          let signed = verify_commit_object c in
          Firehose.verify_commit c;
          match (acct.data, c.prev_data) with
          | Some have, Some want when not (Cid.equal have want) ->
              acct.status <- Desynchronized;
              []
          | _ ->
              let events =
                apply_record_ops ~live acct (Firehose.record_ops c.ops)
              in
              acct.rev <- c.rev;
              acct.data <- Some signed.data;
              acct.commit <- Some c.commit;
              events)

  let process_sync (acct : account) (s : Firehose.sync) : record_change list =
    if s.did <> acct.did then
      fail (Printf.sprintf "sync did %s != account %s" s.did acct.did);
    (match acct.status with
    | Synchronized when acct.rev = s.rev -> ()
    | In_progress -> ()
    | _ -> acct.status <- Desynchronized);
    []

  let process_message ?keys (acct : account) (msg : Firehose.message) :
      record_change list =
    match msg with
    | `Commit c -> process_commit ?keys acct c
    | `Sync s -> process_sync acct s
    | `Identity _ | `Account _ | `Info _ | `Error _ | `Unknown _ -> []

  let resync_from_car ?keys ?(live = false) (acct : account) (car : Car.t) :
      record_change list =
    acct.status <- In_progress;
    let snap = open_car car in
    if snap.did <> acct.did then
      fail (Printf.sprintf "CAR did %s != account %s" snap.did acct.did);
    verify_snapshot ?keys snap;
    let events = diff_walk ~live acct (walk snap) in
    acct.rev <- snap.rev;
    acct.data <- Some snap.data;
    acct.commit <- Some snap.commit_cid;
    let pending = Queue.fold (fun acc c -> c :: acc) [] acct.pending in
    Queue.clear acct.pending;
    acct.status <- Synchronized;
    let rest =
      List.fold_left
        (fun acc c -> acc @ process_commit ?keys ~live:true acct c)
        [] (List.rev pending)
    in
    events @ rest

  let fetch_repo ?host ?session (did : string) : snapshot =
    open_car_bytes (Sync.get_repo ?host ?session did)

  let backfill ?host ?session ?keys (acct : account) : record_change list =
    let snap = fetch_repo ?host ?session acct.did in
    resync_from_car ?keys ~live:false acct snap.car
end
