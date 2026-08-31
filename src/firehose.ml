open Cid
open Dag_cbor
open Car
open Websocket

(** Decoder and WebSocket client for com.atproto.sync.subscribeRepos. *)
module Firehose = struct
  type header = { op : int; t : string option }

  type repo_op = {
    action : string;
    path : string;
    cid : Cid.t option;
    prev : Cid.t option;
  }

  type commit = {
    seq : int64;
    rebase : bool;
    too_big : bool;
    repo : string;
    commit : Cid.t;
    rev : string;
    since : string option;
    prev_data : Cid.t option;
    blocks : Car.t;
    raw_blocks : string;
    ops : repo_op list;
    blobs : Cid.t list;
    time : string;
  }

  type sync = {
    seq : int64;
    did : string;
    blocks : Car.t;
    raw_blocks : string;
    rev : string;
    time : string;
  }

  type identity = {
    seq : int64;
    did : string;
    time : string;
    handle : string option;
  }

  type account = {
    seq : int64;
    did : string;
    time : string;
    active : bool;
    status : string option;
  }

  type info = { name : string; message : string option }

  type message =
    [ `Commit of commit
    | `Sync of sync
    | `Identity of identity
    | `Account of account
    | `Info of info
    | `Error of string * string option
    | `Unknown of string * Dag_cbor.value ]

  let default_relay_host = "bsky.network"

  let subscribe_url ?(host = default_relay_host) ?cursor () =
    let base =
      Printf.sprintf "wss://%s/xrpc/com.atproto.sync.subscribeRepos" host
    in
    match cursor with
    | None -> base
    | Some c -> base ^ "?cursor=" ^ Int64.to_string c

  let parse_header (v : Dag_cbor.value) : header =
    let fields = Dag_cbor.get_map v in
    let op = Dag_cbor.as_int (Dag_cbor.require "op" fields) in
    let t =
      match Dag_cbor.find "t" fields with
      | Some (Dag_cbor.Text s) -> Some s
      | _ -> None
    in
    { op; t }

  let parse_repo_op (v : Dag_cbor.value) : repo_op =
    let fields = Dag_cbor.get_map v in
    {
      action = Dag_cbor.as_text (Dag_cbor.require "action" fields);
      path = Dag_cbor.as_text (Dag_cbor.require "path" fields);
      cid =
        (match Dag_cbor.find "cid" fields with
        | None | Some Dag_cbor.Null -> None
        | Some c -> Some (Dag_cbor.as_cid c));
      prev =
        (match Dag_cbor.find "prev" fields with
        | None | Some Dag_cbor.Null -> None
        | Some c -> Some (Dag_cbor.as_cid c));
    }

  let parse_car_bytes = function
    | Dag_cbor.Bytes b ->
        let car =
          if String.length b = 0 then { Car.roots = []; blocks = [] }
          else Car.parse b
        in
        (car, b)
    | _ -> failwith "Firehose: blocks field must be bytes"

  let parse_commit (v : Dag_cbor.value) : commit =
    let fields = Dag_cbor.get_map v in
    let blocks, raw_blocks =
      parse_car_bytes (Dag_cbor.require "blocks" fields)
    in
    {
      seq = Dag_cbor.as_int64 (Dag_cbor.require "seq" fields);
      rebase =
        (match Dag_cbor.find "rebase" fields with
        | Some b -> Dag_cbor.as_bool b
        | None -> false);
      too_big =
        (match Dag_cbor.find "tooBig" fields with
        | Some b -> Dag_cbor.as_bool b
        | None -> false);
      repo = Dag_cbor.as_text (Dag_cbor.require "repo" fields);
      commit = Dag_cbor.as_cid (Dag_cbor.require "commit" fields);
      rev = Dag_cbor.as_text (Dag_cbor.require "rev" fields);
      since =
        (match Dag_cbor.find "since" fields with
        | Some s -> Dag_cbor.as_text_opt s
        | None -> None);
      prev_data =
        (match Dag_cbor.find "prevData" fields with
        | None | Some Dag_cbor.Null -> None
        | Some c -> Some (Dag_cbor.as_cid c));
      blocks;
      raw_blocks;
      ops =
        (match Dag_cbor.find "ops" fields with
        | Some a -> List.map parse_repo_op (Dag_cbor.as_array a)
        | None -> []);
      blobs =
        (match Dag_cbor.find "blobs" fields with
        | Some a ->
            List.filter_map
              (function Dag_cbor.Null -> None | v -> Some (Dag_cbor.as_cid v))
              (Dag_cbor.as_array a)
        | None -> []);
      time = Dag_cbor.as_text (Dag_cbor.require "time" fields);
    }

  let parse_sync (v : Dag_cbor.value) : sync =
    let fields = Dag_cbor.get_map v in
    let blocks, raw_blocks =
      parse_car_bytes (Dag_cbor.require "blocks" fields)
    in
    {
      seq = Dag_cbor.as_int64 (Dag_cbor.require "seq" fields);
      did = Dag_cbor.as_text (Dag_cbor.require "did" fields);
      blocks;
      raw_blocks;
      rev = Dag_cbor.as_text (Dag_cbor.require "rev" fields);
      time = Dag_cbor.as_text (Dag_cbor.require "time" fields);
    }

  let parse_identity (v : Dag_cbor.value) : identity =
    let fields = Dag_cbor.get_map v in
    {
      seq = Dag_cbor.as_int64 (Dag_cbor.require "seq" fields);
      did = Dag_cbor.as_text (Dag_cbor.require "did" fields);
      time = Dag_cbor.as_text (Dag_cbor.require "time" fields);
      handle =
        (match Dag_cbor.find "handle" fields with
        | Some (Dag_cbor.Text h) -> Some h
        | _ -> None);
    }

  let parse_account (v : Dag_cbor.value) : account =
    let fields = Dag_cbor.get_map v in
    {
      seq = Dag_cbor.as_int64 (Dag_cbor.require "seq" fields);
      did = Dag_cbor.as_text (Dag_cbor.require "did" fields);
      time = Dag_cbor.as_text (Dag_cbor.require "time" fields);
      active = Dag_cbor.as_bool (Dag_cbor.require "active" fields);
      status =
        (match Dag_cbor.find "status" fields with
        | Some (Dag_cbor.Text s) -> Some s
        | _ -> None);
    }

  let parse_info (v : Dag_cbor.value) : info =
    let fields = Dag_cbor.get_map v in
    {
      name = Dag_cbor.as_text (Dag_cbor.require "name" fields);
      message =
        (match Dag_cbor.find "message" fields with
        | Some (Dag_cbor.Text s) -> Some s
        | _ -> None);
    }

  let decode_frame (bytes : string) : header * message =
    match Dag_cbor.decode_sequence bytes with
    | header_v :: body :: _ ->
        let header = parse_header header_v in
        let message =
          if header.op = -1 then
            let fields = Dag_cbor.get_map body in
            let err =
              match Dag_cbor.find "error" fields with
              | Some (Dag_cbor.Text s) -> s
              | _ -> "error"
            in
            let msg =
              match Dag_cbor.find "message" fields with
              | Some (Dag_cbor.Text s) -> Some s
              | _ -> None
            in
            `Error (err, msg)
          else
            match header.t with
            | Some "#commit" -> `Commit (parse_commit body)
            | Some "#sync" -> `Sync (parse_sync body)
            | Some "#identity" -> `Identity (parse_identity body)
            | Some "#account" -> `Account (parse_account body)
            | Some "#info" -> `Info (parse_info body)
            | Some other -> `Unknown (other, body)
            | None -> `Unknown ("", body)
        in
        (header, message)
    | _ ->
        failwith "Firehose.decode_frame: expected header and body CBOR values"

  let encode_header (h : header) : string =
    let fields =
      ("op", Dag_cbor.Int h.op)
      :: (match h.t with Some t -> [ ("t", Dag_cbor.Text t) ] | None -> [])
    in
    Dag_cbor.encode (Dag_cbor.Map fields)

  let subscribe ?(host = default_relay_host) ?cursor ?max_messages f =
    let url = subscribe_url ~host ?cursor () in
    Websocket.with_connection url (fun ws ->
        let rec loop n =
          match max_messages with
          | Some m when n >= m -> ()
          | _ -> (
              match Websocket.recv_message ws with
              | Websocket.Binary payload | Websocket.Text payload ->
                  f (decode_frame payload);
                  loop (n + 1)
              | Websocket.Close _ -> ()
              | Websocket.Ping _ | Websocket.Pong _ -> loop n)
        in
        loop 0)

  let record_ops (ops : repo_op list) : Mst.Mst.record_op list =
    List.map
      (fun (op : repo_op) ->
        {
          Mst.Mst.action = op.action;
          path = op.path;
          cid = op.cid;
          prev = op.prev;
        })
      ops

  let repo_commit_of (c : commit) : Mst.Mst.repo_commit =
    match Car.find_block c.blocks c.commit with
    | None -> failwith "Firehose: commit block missing from CAR"
    | Some block -> Mst.Mst.parse_repo_commit (Dag_cbor.decode block.data)

  let invert_commit (c : commit) : Cid.t =
    if c.too_big then
      failwith "Firehose.invert_commit: tooBig (CAR is incomplete)";
    if c.rebase then failwith "Firehose.invert_commit: rebase is not invertible";
    let repo = repo_commit_of c in
    let store = Mst.Mst.store_of_car c.blocks in
    let tree = Mst.Mst.tree_of_root store repo.data in
    List.iter (Mst.Mst.check_op tree) (record_ops c.ops);
    let inverted = Mst.Mst.invert_ops tree (record_ops c.ops) in
    Mst.Mst.root_cid inverted

  let verify_commit (c : commit) : unit =
    match (c.too_big, c.rebase, c.ops) with
    | true, _, _ | _, true, _ -> ()
    | _, _, [] ->
        (* identity / empty diff: inversion is a no-op when ops are empty. *)
        ()
    | _ -> (
        let inverted = invert_commit c in
        match c.prev_data with
        | Some expected ->
            if not (Cid.equal inverted expected) then
              failwith
                (Printf.sprintf
                   "Firehose.verify_commit: inverted MST %s != prevData %s"
                   (Cid.to_string inverted) (Cid.to_string expected))
        | None -> ())

  let verify_commit_object (c : commit) : Mst.Mst.repo_commit =
    let signed = repo_commit_of c in
    if c.repo <> signed.did then
      failwith
        (Printf.sprintf "Firehose.verify_commit_object: did %s != repo %s"
           signed.did c.repo);
    if c.rev <> signed.rev then
      failwith
        (Printf.sprintf "Firehose.verify_commit_object: rev %s != %s" signed.rev
           c.rev);
    signed

  let verify_commit_sig ~keys (c : commit) : Mst.Mst.sig_status =
    Mst.Mst.verify_commit_sig ~keys (repo_commit_of c)

  let verify_sync (s : sync) : Mst.Mst.repo_commit =
    let root =
      match Car.root s.blocks with
      | Some c -> c
      | None -> failwith "Firehose.verify_sync: CAR has no root"
    in
    let block =
      match Car.find_block s.blocks root with
      | Some b -> b
      | None -> failwith "Firehose.verify_sync: commit block missing"
    in
    let computed = Cid.create block.data in
    if not (Cid.equal computed root) then
      failwith "Firehose.verify_sync: commit CID mismatch";
    let signed = Mst.Mst.parse_repo_commit (Dag_cbor.decode block.data) in
    if signed.did <> s.did then
      failwith
        (Printf.sprintf "Firehose.verify_sync: did %s != %s" signed.did s.did);
    if signed.rev <> s.rev then
      failwith
        (Printf.sprintf "Firehose.verify_sync: rev %s != %s" signed.rev s.rev);
    signed

  let apply_commit ~(prev_tree : Mst.Mst.tree) (c : commit) : Mst.Mst.tree =
    let signed = verify_commit_object c in
    verify_commit c;
    let tree = Mst.Mst.apply_ops prev_tree (record_ops c.ops) in
    let root = Mst.Mst.root_cid tree in
    if not (Cid.equal root signed.data) then
      failwith
        (Printf.sprintf
           "Firehose.apply_commit: applied MST %s != commit.data %s"
           (Cid.to_string root)
           (Cid.to_string signed.data));
    tree

  let subscribe_one ?host ?cursor () : header * message =
    let cell = ref None in
    subscribe ?host ?cursor ~max_messages:1 (fun frame -> cell := Some frame);
    match !cell with
    | Some frame -> frame
    | None -> failwith "Firehose.subscribe_one: no frame received"
end
