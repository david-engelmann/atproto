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

  (** Split [collection/rkey] on the first [/]; no slash yields [path] and
      [""]. *)
  let split_path path =
    match String.index_opt path '/' with
    | None -> (path, "")
    | Some i ->
        ( String.sub path 0 i,
          String.sub path (i + 1) (String.length path - i - 1) )

  (** Open a repo CAR: verify commit CID, rev TID, and MST (full tree when
      [complete]). *)
  let open_car ?(complete = true) (car : Car.t) : snapshot =
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
    if not (Cid.is_blessed ~codec:Cid.Dag_cbor commit_cid) then
      fail "commit CID is not a blessed dag-cbor SHA-256 CID";
    let commit = Mst.parse_repo_commit (Dag_cbor.decode block.data) in
    if commit.version <> 3 then
      fail (Printf.sprintf "unsupported repo commit version %d" commit.version);
    if not (Tid.is_valid commit.rev) then
      fail ("commit rev is not a TID: " ^ commit.rev);
    if Tid.is_future commit.rev then
      fail ("commit rev is in the future: " ^ commit.rev);
    let store = Mst.store_of_car car in
    if complete then
      Mst.verify_tree ~get_block:(Mst.store_get store) commit.data
    else Mst.verify_tree_available ~get_block:(Mst.store_get store) commit.data;
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

  (** [open_car] on CARv1 [bytes]. *)
  let open_car_bytes ?complete (bytes : string) : snapshot =
    open_car ?complete (Car.parse bytes)

  (** Re-verify the MST; optional [keys] check the commit signature. *)
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

  (** All record paths and CIDs in [snap]'s MST. *)
  let walk (snap : snapshot) : (string * Cid.t) list = Mst.walk snap.tree

  (** Record bytes for [cid] in [car]; verifies the CID. *)
  let record_block (car : Car.t) (cid : Cid.t) : string =
    match Car.find_block car cid with
    | None -> fail ("record block missing " ^ Cid.to_string cid)
    | Some b ->
        ignore (Cid.verify_block ~expected:cid ~codec:cid.codec b.data);
        b.data

  (** Partial getRecord proof CAR: return path CID and record bytes. *)
  let verify_record_proof ~(car : Car.t) ~(path : string) : Cid.t * string =
    if not (Syntax.Syntax.is_valid_repo_path path) then
      fail ("invalid repo path " ^ path);
    (* getRecord proof CARs are partial (commit + MST path + record). *)
    let snap = open_car ~complete:false car in
    let store = Mst.store_of_car car in
    match Mst.lookup ~get_block:(Mst.store_get store) snap.data path with
    | None -> fail ("record path not in MST: " ^ path)
    | Some cid -> (cid, record_block car cid)

  (** Commit block plus MST blocks in streamable pre-order. *)
  let preorder_blocks (snap : snapshot) : Car.block list =
    let commit_block =
      match Car.find_block snap.car snap.commit_cid with
      | Some b -> b
      | None ->
          {
            Car.cid = snap.commit_cid;
            data =
              Mst.encode_repo_commit ~did:snap.did ~data:snap.data ~rev:snap.rev
                ?prev:snap.commit.prev ?sig_:snap.commit.sig_ ();
          }
    in
    commit_block :: Mst.preorder_blocks snap.tree

  (** True when [snap.car] blocks follow MST pre-order. *)
  let is_preorder (snap : snapshot) : bool =
    Car.follows_order
      ~expected:(List.map (fun (b : Car.block) -> b.cid) (preorder_blocks snap))
      (Car.block_cids snap.car)

  (** Full repo CAR (commit root, pre-order blocks, first-occurrence only). *)
  let export_car (snap : snapshot) : Car.t =
    let seen = Hashtbl.create 16 in
    let blocks =
      List.filter
        (fun (b : Car.block) ->
          let k = Cid.to_string b.cid in
          if Hashtbl.mem seen k then false
          else (
            Hashtbl.add seen k ();
            true))
        (preorder_blocks snap)
    in
    { Car.roots = [ snap.commit_cid ]; blocks }

  (** CARv1 bytes of [export_car snap]. *)
  let export_car_bytes (snap : snapshot) : string = Car.encode (export_car snap)

  (** Partial CAR for [collections] (commit plus MST range proofs). *)
  let export_subset (snap : snapshot) ~(collections : string list) : Car.t =
    if collections = [] then export_car snap
    else
      let commit_block =
        match Car.find_block snap.car snap.commit_cid with
        | Some b -> b
        | None -> fail "commit block missing from snapshot"
      in
      let blocks =
        List.concat
          (List.map
             (fun collection ->
               let start, end_exclusive = Mst.collection_range collection in
               Mst.range_blocks ~start ~end_exclusive snap.tree)
             collections)
      in
      let all = commit_block :: blocks in
      let expected =
        Car.first_occurrences (List.map (fun (b : Car.block) -> b.cid) all)
      in
      {
        Car.roots = [ snap.commit_cid ];
        blocks =
          List.filter_map
            (fun cid ->
              List.find_opt (fun (b : Car.block) -> Cid.equal b.cid cid) all)
            expected;
      }

  (** CARv1 bytes of [export_subset]. *)
  let export_subset_bytes snap ~collections =
    Car.encode (export_subset snap ~collections)

  exception Not_preorder of string

  (** Walk records by consuming blocks in streamable pre-order. Omitted child
      CIDs (subset proofs) are skipped; an out-of-order present block fails. *)
  let stream_walk (car : Car.t) : (string * Cid.t) list =
    let by_cid = Hashtbl.create (List.length car.blocks) in
    List.iter
      (fun (b : Car.block) ->
        let k = Cid.to_string b.cid in
        if not (Hashtbl.mem by_cid k) then Hashtbl.add by_cid k b)
      car.blocks;
    let unused = Queue.create () in
    List.iter (fun b -> Queue.add b unused) car.blocks;
    let consumed = Hashtbl.create 16 in
    let take (cid : Cid.t) : Car.block option =
      let k = Cid.to_string cid in
      if Hashtbl.mem consumed k then Hashtbl.find_opt by_cid k
      else if not (Hashtbl.mem by_cid k) then None
      else
        let rec skip () =
          if Queue.is_empty unused then
            raise (Not_preorder ("expected " ^ Cid.to_string cid))
          else
            let next = Queue.take unused in
            let nk = Cid.to_string next.cid in
            if Hashtbl.mem consumed nk then skip ()
            else if Cid.equal next.cid cid then (
              Hashtbl.add consumed k ();
              Some next)
            else
              raise
                (Not_preorder
                   (Printf.sprintf "pre-order expected %s got %s"
                      (Cid.to_string cid) (Cid.to_string next.cid)))
        in
        skip ()
    in
    let commit_cid =
      match Car.root car with Some c -> c | None -> fail "CAR has no root"
    in
    (match take commit_cid with
    | None -> fail "commit block missing from CAR"
    | Some _ -> ());
    let commit_block =
      match Hashtbl.find_opt by_cid (Cid.to_string commit_cid) with
      | Some b -> b
      | None -> fail "commit block missing from CAR"
    in
    let commit = Mst.parse_repo_commit (Dag_cbor.decode commit_block.data) in
    let rec walk_node (cid : Cid.t) : (string * Cid.t) list =
      match take cid with
      | None -> []
      | Some block ->
          let node = Mst.node_of_bytes block.data in
          let items = Mst.verify_node node in
          let left =
            match node.left with Some c -> walk_node c | None -> []
          in
          left
          @ List.concat
              (List.map
                 (fun (r : Mst.reconstructed) ->
                   (match take r.value with Some _ | None -> ());
                   (r.key, r.value)
                   :: (match r.right with Some c -> walk_node c | None -> []))
                 items)
    in
    walk_node commit.data

  (** Walk records: streamable pre-order, or [Mst.walk_available] if
      shuffled. *)
  let walk_car (car : Car.t) : (string * Cid.t) list =
    try stream_walk car
    with Not_preorder _ ->
      let snap = open_car ~complete:false car in
      Mst.walk_available snap.tree

  (** Check collection range proofs and record blocks in a subset CAR. *)
  let verify_subset ~(collections : string list) (car : Car.t) : snapshot =
    let snap = open_car ~complete:false car in
    let store = Mst.store_of_car car in
    List.iter
      (fun collection ->
        let start, end_exclusive = Mst.collection_range collection in
        (match
           Mst.range_completeness ~start ~end_exclusive
             ~get_block:(Mst.store_get store) snap.data
         with
        | Mst.Complete -> ()
        | Mst.Incomplete msg -> fail ("subset proof incomplete: " ^ msg));
        List.iter
          (fun (path, cid) ->
            if Mst.key_in_range ~start ~end_exclusive path then
              ignore (record_block car cid))
          (Mst.walk_available snap.tree))
      collections;
    snap

  (** In-memory TAP account for [did] (starts [Desynchronized]). *)
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

  (** Check firehose commit DID/rev against the signed object. *)
  let verify_commit_object (c : Firehose.commit) : Mst.repo_commit =
    let signed = Firehose.repo_commit_of c in
    if c.repo <> signed.did then
      fail (Printf.sprintf "commit.did %s != repo %s" signed.did c.repo);
    if c.rev <> signed.rev then
      fail (Printf.sprintf "commit.rev %s != rev %s" signed.rev c.rev);
    signed

  (** Verify the firehose commit signature with PLC / did:key [keys]. *)
  let verify_commit_sig ~keys (c : Firehose.commit) : unit =
    let signed = Firehose.repo_commit_of c in
    match Mst.verify_commit_sig ~keys signed with
    | `Valid -> ()
    | `Missing -> fail "firehose commit is unsigned"
    | `Invalid -> fail "firehose commit signature is invalid"
    | `Unsupported_curve curve ->
        fail ("firehose commit uses unsupported curve " ^ curve)

  (** Apply [c]'s ops to [prev] and check the resulting MST root. *)
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

  (** Apply a live [#commit] to [acct], or queue / desync as needed. *)
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

  (** Handle a [#sync] frame: desync unless [acct] already matches [s.rev]. *)
  let process_sync (acct : account) (s : Firehose.sync) : record_change list =
    if s.did <> acct.did then
      fail (Printf.sprintf "sync did %s != account %s" s.did acct.did);
    (match acct.status with
    | Synchronized when acct.rev = s.rev -> ()
    | In_progress -> ()
    | _ -> acct.status <- Desynchronized);
    []

  (** Dispatch a firehose [msg] ([#commit] / [#sync]; others ignored). *)
  let process_message ?keys (acct : account) (msg : Firehose.message) :
      record_change list =
    match msg with
    | `Commit c -> process_commit ?keys acct c
    | `Sync s -> process_sync acct s
    | `Identity _ | `Account _ | `Info _ | `Error _ | `Unknown _ -> []

  (** Backfill [acct] from a full repo CAR, then replay queued commits. *)
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

  (** Full repo snapshot via [com.atproto.sync.getRepo]. *)
  let fetch_repo ?host ?session (did : string) : snapshot =
    open_car_bytes (Sync.get_repo ?host ?session did)

  (** Subset CAR for [collections] from a fetched repo. *)
  let fetch_repo_subset ?host ?session ~did ~collections () : Car.t =
    let snap = fetch_repo ?host ?session did in
    export_subset snap ~collections

  (** getRecord proof via [com.atproto.sync.getRecord]; verify path. *)
  let fetch_record_proof ?host ?session ?commit ~did ~collection ~rkey () :
      Cid.t * string =
    let car = Sync.get_record_car ?host ?session ?commit did collection rkey in
    verify_record_proof ~car ~path:(Sync.record_path ~collection ~rkey)

  (** Fetch the repo and [resync_from_car] into [acct]. *)
  let backfill ?host ?session ?keys (acct : account) : record_change list =
    let snap = fetch_repo ?host ?session acct.did in
    resync_from_car ?keys ~live:false acct snap.car

  type commit_signer =
    did:string -> data:Cid.t -> rev:string -> ?prev:Cid.t -> unit -> string

  (** Offline signed repo: JSON records → DAG-CBOR → MST → commit → CAR. *)
  let write_signed_repo ~did ~rev ?prev ~(sign : commit_signer)
      ~(records : (string * Yojson.Safe.t) list) () : snapshot =
    List.iter
      (fun (path, _) ->
        if not (Syntax.Syntax.is_valid_repo_path path) then
          fail ("write_signed_repo: invalid repo path " ^ path))
      records;
    if not (Tid.is_valid rev) then fail ("write_signed_repo: invalid rev " ^ rev);
    let store = Mst.store_of_get (fun _ -> None) in
    let tree = Mst.empty_tree store in
    let record_blocks = ref [] in
    let tree =
      List.fold_left
        (fun t (path, json) ->
          let bytes = Dag_cbor.encode (Dag_cbor.of_yojson json) in
          let cid = Cid.create bytes in
          record_blocks := { Car.cid; data = bytes } :: !record_blocks;
          fst (Mst.insert t path cid))
        tree records
    in
    let data = Mst.root_cid tree in
    let commit_bytes = sign ~did ~data ~rev ?prev () in
    let commit_cid = Cid.create commit_bytes in
    let mst_blocks =
      Hashtbl.fold
        (fun k block_data acc ->
          { Car.cid = Cid.of_string k; data = block_data } :: acc)
        tree.store.created []
    in
    let seen = Hashtbl.create 16 in
    let blocks =
      List.filter
        (fun (b : Car.block) ->
          let k = Cid.to_string b.cid in
          if Hashtbl.mem seen k then false
          else (
            Hashtbl.add seen k ();
            true))
        (({ Car.cid = commit_cid; data = commit_bytes } :: mst_blocks)
        @ !record_blocks)
    in
    let car = { Car.roots = [ commit_cid ]; blocks } in
    open_car car
end
