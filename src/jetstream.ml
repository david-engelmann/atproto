open Websocket

(** Jetstream JSON firehose — live tail for v2 (recommended) and v1.

    Spec: https://bsky.network/docs/jetstream/
    Lexicon: network.bsky.jetstream.subscribeEvents (xrpc.v1.json)

    Network Replay / snapshot HTTP methods are typed here but are not called
    live (public instances gate the archive; this library does not invent a
    token). *)
module Jetstream = struct
  let v2_west_host = "jetstream.us-west.bsky.network"
  let v2_east_host = "jetstream.us-east.bsky.network"
  let v1_west_host = "jetstream1.us-west.bsky.network"
  let v1_east_host = "jetstream2.us-east.bsky.network"
  let default_host = v2_west_host
  let v2_path = "/xrpc/network.bsky.jetstream.subscribeEvents"
  let v1_path = "/subscribe"
  let subscribe_nsid = "network.bsky.jetstream.subscribeEvents"
  let max_collections = 100
  let max_dids = 10_000
  let time_us_floor = 1_000_000_000_000_000L

  type version = V1 | V2
  type kind = Commit | Identity | Account | Sync
  type cursor = Seq of int64 | Time_us of int64

  type filter = {
    collections : string list;
    dids : string list;
    kinds : kind list;
    cursor : cursor option;
    max_message_size_bytes : int option;
  }

  let empty_filter =
    {
      collections = [];
      dids = [];
      kinds = [];
      cursor = None;
      max_message_size_bytes = None;
    }

  let kind_to_string = function
    | Commit -> "commit"
    | Identity -> "identity"
    | Account -> "account"
    | Sync -> "sync"

  let kind_of_string = function
    | "commit" -> Some Commit
    | "identity" -> Some Identity
    | "account" -> Some Account
    | "sync" -> Some Sync
    | _ -> None

  exception Invalid_filter of string

  let validate_filter (f : filter) : unit =
    if List.length f.collections > max_collections then
      raise
        (Invalid_filter (Printf.sprintf "collections cap is %d" max_collections));
    if List.length f.dids > max_dids then
      raise (Invalid_filter (Printf.sprintf "dids cap is %d" max_dids));
    List.iter
      (fun d ->
        if not (Syntax.Syntax.is_valid_did d) then
          raise (Invalid_filter ("invalid did " ^ d)))
      f.dids;
    if f.collections <> [] && f.kinds <> [] && not (List.mem Commit f.kinds)
    then
      raise
        (Invalid_filter
           "collections filter requires kinds to include commit (or omit kinds)")

  let cursor_of_int64 (n : int64) : cursor =
    if n >= time_us_floor then Time_us n else Seq n

  let cursor_to_string = function Seq n | Time_us n -> Int64.to_string n
  let repeat_params key values = List.map (fun v -> (key, v)) values

  let filter_pairs ?(version = V2) (f : filter) : (string * string) list =
    let coll_key, did_key =
      match version with
      | V2 -> ("collections", "dids")
      | V1 -> ("wantedCollections", "wantedDids")
    in
    repeat_params coll_key f.collections
    @ repeat_params did_key f.dids
    @ (match version with
      | V2 -> repeat_params "kinds" (List.map kind_to_string f.kinds)
      | V1 -> [])
    @ (match f.cursor with
      | Some c -> [ ("cursor", cursor_to_string c) ]
      | None -> [])
    @
    match (version, f.max_message_size_bytes) with
    | V2, Some n when n > 0 -> [ ("maxMessageSizeBytes", string_of_int n) ]
    | _ -> []

  let subscribe_url ?(host = default_host) ?(version = V2)
      ?(filter = empty_filter) () =
    validate_filter filter;
    let path = match version with V2 -> v2_path | V1 -> v1_path in
    let qs =
      Cohttp_client.Cohttp_client.create_body_from_pairs
        (filter_pairs ~version filter)
    in
    let base = Printf.sprintf "wss://%s%s" host path in
    if qs = "" then base else base ^ "?" ^ qs

  type commit = {
    did : string;
    seq : int64;
    time : string;
    operation : string;
    collection : string;
    rkey : string;
    rev : string;
    cid : string option;
    record : Yojson.Safe.t option;
  }

  type identity = {
    did : string;
    seq : int64;
    time : string;
    handle : string option;
  }

  type account = {
    did : string;
    seq : int64;
    time : string;
    active : bool;
    status : string option;
  }

  type sync = { did : string; seq : int64; time : string; rev : string }
  type info = { name : string; message : string option }

  type event =
    [ `Commit of commit
    | `Identity of identity
    | `Account of account
    | `Sync of sync
    | `Info of info
    | `Unknown of Yojson.Safe.t ]

  let string_member json field =
    match Yojson.Safe.Util.member field json with `String s -> s | _ -> ""

  let string_opt json field =
    match Yojson.Safe.Util.member field json with
    | `String s -> Some s
    | _ -> None

  let bool_member json field =
    match Yojson.Safe.Util.member field json with `Bool b -> b | _ -> false

  let int64_member json field =
    match Yojson.Safe.Util.member field json with
    | `Int n -> Int64.of_int n
    | `Intlit s -> Int64.of_string s
    | `String s -> ( try Int64.of_string s with _ -> 0L)
    | _ -> 0L

  let payload_type_fragment typ =
    match String.rindex_opt typ '#' with
    | None -> typ
    | Some i -> String.sub typ (i + 1) (String.length typ - i - 1)

  let parse_commit json : commit =
    {
      did = string_member json "did";
      seq = int64_member json "seq";
      time = string_member json "time";
      operation = string_member json "operation";
      collection = string_member json "collection";
      rkey = string_member json "rkey";
      rev = string_member json "rev";
      cid = string_opt json "cid";
      record =
        (match Yojson.Safe.Util.member "record" json with
        | `Null -> None
        | other -> Some other);
    }

  let parse_identity json : identity =
    let inner =
      match Yojson.Safe.Util.member "identity" json with
      | `Assoc _ as obj -> obj
      | _ -> json
    in
    {
      did = string_member json "did";
      seq = int64_member json "seq";
      time = string_member json "time";
      handle =
        (match string_opt inner "handle" with
        | Some _ as h -> h
        | None -> string_opt json "handle");
    }

  let parse_account json : account =
    let inner =
      match Yojson.Safe.Util.member "account" json with
      | `Assoc _ as obj -> obj
      | _ -> json
    in
    {
      did = string_member json "did";
      seq = int64_member json "seq";
      time = string_member json "time";
      active =
        (match Yojson.Safe.Util.member "active" inner with
        | `Bool b -> b
        | _ -> bool_member json "active");
      status =
        (match string_opt inner "status" with
        | Some _ as s -> s
        | None -> string_opt json "status");
    }

  let parse_sync json : sync =
    let inner =
      match Yojson.Safe.Util.member "sync" json with
      | `Assoc _ as obj -> obj
      | _ -> json
    in
    {
      did = string_member json "did";
      seq = int64_member json "seq";
      time = string_member json "time";
      rev =
        (match string_opt inner "rev" with
        | Some r -> r
        | None -> string_member json "rev");
    }

  let parse_info json : info =
    { name = string_member json "name"; message = string_opt json "message" }

  let parse_v1_commit did time_us json : commit =
    {
      did;
      seq = time_us;
      time = "";
      operation = string_member json "operation";
      collection = string_member json "collection";
      rkey = string_member json "rkey";
      rev = string_member json "rev";
      cid = string_opt json "cid";
      record =
        (match Yojson.Safe.Util.member "record" json with
        | `Null -> None
        | other -> Some other);
    }

  let parse_kind_payload kind json : event =
    match kind with
    | "commit" -> `Commit (parse_commit json)
    | "identity" -> `Identity (parse_identity json)
    | "account" -> `Account (parse_account json)
    | "sync" -> `Sync (parse_sync json)
    | "info" -> `Info (parse_info json)
    | _ -> `Unknown json

  let parse_event json : event =
    match Yojson.Safe.Util.member "payload" json with
    | `Assoc _ as payload ->
        let frag = payload_type_fragment (string_member payload "$type") in
        parse_kind_payload frag payload
    | _ -> (
        match string_opt json "kind" with
        | Some kind -> (
            let did = string_member json "did" in
            let time_us = int64_member json "time_us" in
            match kind with
            | "commit" ->
                `Commit
                  (parse_v1_commit did time_us
                     (Yojson.Safe.Util.member "commit" json))
            | "identity" ->
                `Identity
                  (parse_identity
                     (match Yojson.Safe.Util.member "identity" json with
                     | `Assoc _ as inner -> inner
                     | _ -> json))
            | "account" ->
                `Account
                  (parse_account
                     (match Yojson.Safe.Util.member "account" json with
                     | `Assoc _ as inner -> inner
                     | _ -> json))
            | "sync" -> `Sync (parse_sync json)
            | _ -> `Unknown json)
        | None ->
            let typ = payload_type_fragment (string_member json "$type") in
            if typ = "" then `Unknown json else parse_kind_payload typ json)

  let parse_frame (body : string) : event =
    parse_event (Yojson.Safe.from_string body)

  let seq_of (ev : event) : int64 option =
    match ev with
    | `Commit (c : commit) -> Some c.seq
    | `Identity (i : identity) -> Some i.seq
    | `Account (a : account) -> Some a.seq
    | `Sync (s : sync) -> Some s.seq
    | `Info _ | `Unknown _ -> None

  let event_key (ev : event) : string =
    match ev with
    | `Commit (c : commit) ->
        Printf.sprintf "c:%s/%s/%s@%Ld" c.did c.collection c.rkey c.seq
    | `Identity (i : identity) -> Printf.sprintf "i:%s@%Ld" i.did i.seq
    | `Account (a : account) -> Printf.sprintf "a:%s@%Ld" a.did a.seq
    | `Sync (s : sync) -> Printf.sprintf "s:%s@%Ld" s.did s.seq
    | `Info (i : info) -> "info:" ^ i.name
    | `Unknown _ -> "unknown"

  let record_uri (c : commit) : string =
    Printf.sprintf "at://%s/%s/%s" c.did c.collection c.rkey

  type seen = {
    keys : (string, unit) Hashtbl.t;
    order : string Queue.t;
    cap : int;
  }

  let create_seen ?(cap = 4096) () : seen =
    { keys = Hashtbl.create cap; order = Queue.create (); cap }

  let remember (s : seen) (ev : event) : unit =
    let key = event_key ev in
    if not (Hashtbl.mem s.keys key) then (
      Hashtbl.add s.keys key ();
      Queue.add key s.order;
      if Queue.length s.order > s.cap then
        let old = Queue.take s.order in
        Hashtbl.remove s.keys old)

  let is_duplicate (s : seen) (ev : event) : bool =
    Hashtbl.mem s.keys (event_key ev)

  let with_cursor (f : filter) (c : cursor) : filter =
    { f with cursor = Some c }

  let subscribe ?(host = default_host) ?(version = V2) ?(filter = empty_filter)
      ?max_messages ?(max_reconnects = 0)
      ?(sleep = fun n -> Unix.sleepf (min 8.0 (2.0 ** float_of_int n))) f =
    validate_filter filter;
    let filter = ref filter in
    let seen = create_seen () in
    let received = ref 0 in
    let rec attempt n =
      let url = subscribe_url ~host ~version ~filter:!filter () in
      try
        Websocket.with_connection url (fun ws ->
            let rec loop () =
              match max_messages with
              | Some m when !received >= m -> ()
              | _ -> (
                  match Websocket.recv_message ws with
                  | Websocket.Text payload | Websocket.Binary payload ->
                      let ev = parse_frame payload in
                      (match seq_of ev with
                      | Some s -> filter := with_cursor !filter (Seq s)
                      | None -> ());
                      if not (is_duplicate seen ev) then (
                        remember seen ev;
                        incr received;
                        f ev);
                      loop ()
                  | Websocket.Close _ -> ()
                  | Websocket.Ping _ | Websocket.Pong _ -> loop ())
            in
            loop ())
      with exn ->
        if n >= max_reconnects then raise exn
        else (
          sleep n;
          attempt (n + 1))
    in
    attempt 0

  let subscribe_one ?host ?version ?filter () : event =
    let cell = ref None in
    subscribe ?host ?version ?filter ~max_messages:1 ~max_reconnects:0
      (fun ev -> cell := Some ev);
    match !cell with
    | Some ev -> ev
    | None -> failwith "Jetstream.subscribe_one: no event received"

  (* ---- HTTP snapshot / replay (typed; no invented archive token) -------- *)

  type block_range = { first : int; last : int }

  type snapshot_segment = {
    name : string;
    index : int;
    checksum : string;
    min_seq : int64;
    max_seq : int64;
    mode : string;
    blocks : block_range list;
  }

  type snapshot_stats = {
    segments_examined : int;
    segments_matched : int;
    blocks_matched : int;
    entries : int;
  }

  type snapshot_plan = {
    planned_through_seq : int64;
    sealed_tip_seq : int64;
    segments : snapshot_segment list;
    stats : snapshot_stats;
  }

  let plan_snapshot_url ?(host = default_host) () =
    Printf.sprintf "https://%s/xrpc/network.bsky.jetstream.planSnapshot" host

  let get_segment_url ?(host = default_host) ~name () =
    let qs =
      Cohttp_client.Cohttp_client.create_body_from_pairs [ ("name", name) ]
    in
    Printf.sprintf "https://%s/xrpc/network.bsky.jetstream.getSegment?%s" host
      qs

  let get_block_url ?(host = default_host) ~name ~index () =
    let qs =
      Cohttp_client.Cohttp_client.create_body_from_pairs
        [ ("name", name); ("index", string_of_int index) ]
    in
    Printf.sprintf "https://%s/xrpc/network.bsky.jetstream.getBlock?%s" host qs

  let plan_snapshot_body ?kinds ?dids ?collections ?after_seq ?before_seq () :
      Yojson.Safe.t =
    let list_field key = function
      | None | Some [] -> []
      | Some xs -> [ (key, `List (List.map (fun s -> `String s) xs)) ]
    in
    let int64_field key = function
      | None -> []
      | Some n -> [ (key, `Intlit (Int64.to_string n)) ]
    in
    `Assoc
      (list_field "kinds" kinds @ list_field "dids" dids
      @ list_field "collections" collections
      @ int64_field "afterSeq" after_seq
      @ int64_field "beforeSeq" before_seq)

  let parse_block_range json : block_range =
    {
      first =
        (match Yojson.Safe.Util.member "first" json with `Int n -> n | _ -> 0);
      last =
        (match Yojson.Safe.Util.member "last" json with `Int n -> n | _ -> 0);
    }

  let parse_snapshot_segment json : snapshot_segment =
    {
      name = string_member json "name";
      index =
        (match Yojson.Safe.Util.member "index" json with `Int n -> n | _ -> 0);
      checksum = string_member json "checksum";
      min_seq = int64_member json "minSeq";
      max_seq = int64_member json "maxSeq";
      mode = string_member json "mode";
      blocks =
        (match Yojson.Safe.Util.member "blocks" json with
        | `List xs -> List.map parse_block_range xs
        | _ -> []);
    }

  let parse_snapshot_stats json : snapshot_stats =
    let int_field field =
      match Yojson.Safe.Util.member field json with `Int n -> n | _ -> 0
    in
    {
      segments_examined = int_field "segmentsExamined";
      segments_matched = int_field "segmentsMatched";
      blocks_matched = int_field "blocksMatched";
      entries = int_field "entries";
    }

  let parse_snapshot_plan json : snapshot_plan =
    {
      planned_through_seq = int64_member json "plannedThroughSeq";
      sealed_tip_seq = int64_member json "sealedTipSeq";
      segments =
        (match Yojson.Safe.Util.member "segments" json with
        | `List xs -> List.map parse_snapshot_segment xs
        | _ -> []);
      stats = parse_snapshot_stats (Yojson.Safe.Util.member "stats" json);
    }

  (* Live archive HTTP. Public hosts gate this; pass [token] only if the
     operator already has one. This library never invents a token. Live tail
     ([subscribe]) stays unauthenticated. *)
  exception Snapshot_gated of int * string
  exception Snapshot_http of int * string

  type snapshot_fetch =
    [ `Plan of snapshot_plan | `Bytes of string | `Gated of int * string ]

  let snapshot_headers ?token () =
    let pairs =
      Cohttp_client.Cohttp_client.application_json_setting_tuple
      ::
      (match token with
      | Some t when t <> "" -> [ ("Authorization", "Bearer " ^ t) ]
      | _ -> [])
    in
    Cohttp_client.Cohttp_client.create_headers_from_pairs pairs

  let classify_snapshot_status code body =
    if code = 401 || code = 403 then raise (Snapshot_gated (code, body))
    else if code >= 400 then raise (Snapshot_http (code, body))
    else body

  let try_plan_snapshot ?host ?token ?kinds ?dids ?collections ?after_seq
      ?before_seq () : snapshot_plan =
    let url = plan_snapshot_url ?host () in
    let headers = snapshot_headers ?token () in
    let data =
      Yojson.Safe.to_string
        (plan_snapshot_body ?kinds ?dids ?collections ?after_seq ?before_seq ())
    in
    let code, body =
      Lwt_main.run
        (Cohttp_client.Cohttp_client.post_with_status url data headers)
    in
    parse_snapshot_plan
      (Yojson.Safe.from_string (classify_snapshot_status code body))

  let try_get_segment ?host ?token ~name () : string =
    let url = get_segment_url ?host ~name () in
    let headers = snapshot_headers ?token () in
    let code, body =
      Lwt_main.run (Cohttp_client.Cohttp_client.get_with_status url headers)
    in
    classify_snapshot_status code body

  let try_get_block ?host ?token ~name ~index () : string =
    let url = get_block_url ?host ~name ~index () in
    let headers = snapshot_headers ?token () in
    let code, body =
      Lwt_main.run (Cohttp_client.Cohttp_client.get_with_status url headers)
    in
    classify_snapshot_status code body
end
