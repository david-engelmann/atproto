open Websocket

(** Jetstream JSON firehose — live tail for v2 (recommended) and v1.

    Spec: https://bsky.network/docs/jetstream/
    Lexicon: network.bsky.jetstream.subscribeEvents (xrpc.v1.json)

    Live [subscribe] with [~compress:true] requests dict-zstd frames.
    v2 sends [zstdDictionary=<id>] (the subscribeEvents opt-in). v1 sends
    [compress=true] and [Socket-Encoding: zstd]. The dictionary is fetched
    from [network.bsky.jetstream.getZstdDictionary]; a checked-in copy of
    the production blob (id 20260811) is the fallback if that GET fails.

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
  exception Unknown_zstd_dictionary of int * int option
  exception Zstd_decode of string
  exception Dictionary_http of int * string

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
      ?(filter = empty_filter) ?(compress = false) ?zstd_dictionary_id () =
    validate_filter filter;
    let path = match version with V2 -> v2_path | V1 -> v1_path in
    let extra =
      match (version, compress, zstd_dictionary_id) with
      | V2, true, Some id when id > 0 ->
          [ ("zstdDictionary", string_of_int id) ]
      | V1, true, _ -> [ ("compress", "true") ]
      | _ -> []
    in
    let qs =
      Cohttp_client.Cohttp_client.create_body_from_pairs
        (filter_pairs ~version filter @ extra)
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

  (* ---- dict-zstd (live subscribeEvents + optional .jss callback) -------- *)

  let get_zstd_dictionary_nsid = "network.bsky.jetstream.getZstdDictionary"
  let embedded_zstd_dictionary = Jetstream_zstd_dictionary.bytes
  let max_zstd_frame = 16 * 1024 * 1024
  let zstd_dict_magic = "\x37\xa4\x30\xec"
  let zstd_frame_magic = "\x28\xb5\x2f\xfd"

  let zstd_u32_le s i =
    Char.code s.[i]
    lor (Char.code s.[i + 1] lsl 8)
    lor (Char.code s.[i + 2] lsl 16)
    lor (Char.code s.[i + 3] lsl 24)

  let zstd_dictionary_id (blob : string) : int option =
    if String.length blob < 8 then None
    else if String.sub blob 0 4 <> zstd_dict_magic then None
    else Some (zstd_u32_le blob 4)

  let zstd_frame_dict_id (frame : string) : int option =
    if String.length frame < 5 then None
    else if String.sub frame 0 4 <> zstd_frame_magic then None
    else
      let desc = Char.code frame.[4] in
      let dict_flag = desc land 0x3 in
      let single_segment = desc land 0x20 <> 0 in
      let off = 5 + if single_segment then 0 else 1 in
      match dict_flag with
      | 0 -> None
      | 1 when String.length frame > off -> Some (Char.code frame.[off])
      | 2 when String.length frame > off + 1 ->
          Some (Char.code frame.[off] lor (Char.code frame.[off + 1] lsl 8))
      | 3 when String.length frame > off + 3 -> Some (zstd_u32_le frame off)
      | _ -> None

  let get_zstd_dictionary_url ?(host = default_host) ?id () =
    let base =
      Printf.sprintf "https://%s/xrpc/%s" host get_zstd_dictionary_nsid
    in
    match id with
    | Some n when n > 0 ->
        let qs =
          Cohttp_client.Cohttp_client.create_body_from_pairs
            [ ("id", string_of_int n) ]
        in
        base ^ "?" ^ qs
    | _ -> base

  let try_get_zstd_dictionary ?host ?id () : string =
    let url = get_zstd_dictionary_url ?host ?id () in
    let headers = Cohttp_client.Cohttp_client.create_headers_from_pairs [] in
    let code, body =
      Lwt_main.run (Cohttp_client.Cohttp_client.get_with_status url headers)
    in
    if code < 200 || code >= 300 then raise (Dictionary_http (code, body));
    if zstd_dictionary_id body = None then
      raise (Zstd_decode "getZstdDictionary response is not a zstd dictionary");
    body

  let load_zstd_dictionary ?host () : string =
    try try_get_zstd_dictionary ?host () with _ -> embedded_zstd_dictionary

  let is_unknown_zstd_dictionary body =
    match Error.Error.of_body body with
    | Some e -> e.error = "UnknownZstdDictionary"
    | None -> false

  let decompress_zstd ?(dictionary = embedded_zstd_dictionary) (frame : string)
      : string =
    (match (zstd_frame_dict_id frame, zstd_dictionary_id dictionary) with
    | Some fid, Some did when fid <> did ->
        raise (Unknown_zstd_dictionary (fid, Some did))
    | _ -> ());
    let ctx = Zstandard.Decompression_context.create () in
    Fun.protect
      ~finally:(fun () -> Zstandard.Decompression_context.free ctx)
      (fun () ->
        try
          Zstandard.Simple_dictionary.decompress ctx
            ~dictionary:(Zstandard.Input.from_string dictionary)
            ~input:(Zstandard.Input.from_string frame)
            ~output:
              (Zstandard.Output.allocate_string
                 ~size_limit:(Some max_zstd_frame))
        with
        | Unknown_zstd_dictionary _ as exn -> raise exn
        | Zstd_decode _ as exn -> raise exn
        | Zstandard.Error msg -> raise (Zstd_decode msg)
        | Zstandard.Content_size_unknown ->
            raise (Zstd_decode "zstd frame omitted decompressed size")
        | Zstandard.Content_size_error ->
            raise (Zstd_decode "zstd frame header is invalid")
        | Zstandard.Not_enough_capacity n ->
            raise
              (Zstd_decode
                 (Printf.sprintf "zstd frame exceeds cap (%d bytes)" n))
        | Zstandard.Decompressed_size_exceeds_max_int n ->
            raise
              (Zstd_decode (Printf.sprintf "zstd frame too large (%Ld bytes)" n))
        | exn -> raise (Zstd_decode (Printexc.to_string exn)))

  let compress_zstd ?(dictionary = embedded_zstd_dictionary) (plain : string) :
      string =
    let ctx = Zstandard.Compression_context.create () in
    Fun.protect
      ~finally:(fun () -> Zstandard.Compression_context.free ctx)
      (fun () ->
        Zstandard.Simple_dictionary.compress ctx ~compression_level:3
          ~dictionary:(Zstandard.Input.from_string dictionary)
          ~input:(Zstandard.Input.from_string plain)
          ~output:(Zstandard.Output.allocate_string ~size_limit:None))

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
      ?(compress = false) ?max_messages ?(max_reconnects = 0)
      ?(sleep = fun n -> Unix.sleepf (min 8.0 (2.0 ** float_of_int n))) f =
    validate_filter filter;
    let filter = ref filter in
    let seen = create_seen () in
    let received = ref 0 in
    let dict_blob =
      ref (if compress then Some (load_zstd_dictionary ~host ()) else None)
    in
    let refetched = ref false in
    let rec attempt n =
      let dict_id =
        match !dict_blob with None -> None | Some d -> zstd_dictionary_id d
      in
      let url =
        subscribe_url ~host ~version ~filter:!filter ~compress
          ?zstd_dictionary_id:dict_id ()
      in
      let extra_headers =
        match (version, compress) with
        | V1, true -> [ ("Socket-Encoding", "zstd") ]
        | _ -> []
      in
      try
        Websocket.with_connection ~extra_headers url (fun ws ->
            let rec loop () =
              match max_messages with
              | Some m when !received >= m -> ()
              | _ -> (
                  match Websocket.recv_message ws with
                  | Websocket.Text payload ->
                      let ev = parse_frame payload in
                      (match seq_of ev with
                      | Some s -> filter := with_cursor !filter (Seq s)
                      | None -> ());
                      if not (is_duplicate seen ev) then (
                        remember seen ev;
                        incr received;
                        f ev);
                      loop ()
                  | Websocket.Binary payload ->
                      let body =
                        if compress then
                          match !dict_blob with
                          | Some d -> decompress_zstd ~dictionary:d payload
                          | None ->
                              raise
                                (Zstd_decode
                                   "compressed binary frame without a \
                                    dictionary")
                        else payload
                      in
                      let ev = parse_frame body in
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
      with
      | Websocket.Handshake_error (_, body) as exn
        when compress && (not !refetched) && is_unknown_zstd_dictionary body ->
          refetched := true;
          (try dict_blob := Some (try_get_zstd_dictionary ~host ())
           with _ -> raise exn);
          attempt n
      | exn ->
          if n >= max_reconnects then raise exn
          else (
            sleep n;
            attempt (n + 1))
    in
    attempt 0

  let subscribe_one ?host ?version ?filter ?(compress = false) () : event =
    let cell = ref None in
    subscribe ?host ?version ?filter ~compress ~max_messages:1 ~max_reconnects:0
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

  (* listSegments row — snapshot-only archive mirror (HTTP + token). *)
  type archive_segment = {
    name : string;
    index : int;
    checksum : string;
    size_bytes : int64 option;
    event_count : int option;
    min_seq : int64;
    max_seq : int64;
    min_witnessed_at : int64 option;
    max_witnessed_at : int64 option;
  }

  type list_segments = {
    cursor : string option;
    segments : archive_segment list;
  }

  type download_job =
    | Segment of { name : string; checksum : string }
    | Blocks of { name : string; checksum : string; ranges : block_range list }

  let plan_snapshot_url ?(host = default_host) () =
    Printf.sprintf "https://%s/xrpc/network.bsky.jetstream.planSnapshot" host

  let plan_backfill_url ?(host = default_host) () =
    Printf.sprintf "https://%s/xrpc/network.bsky.jetstream.planBackfill" host

  let list_segments_url ?(host = default_host) ?cursor () =
    let qs =
      Cohttp_client.Cohttp_client.create_body_from_pairs
        (match cursor with Some c -> [ ("cursor", c) ] | None -> [])
    in
    let base =
      Printf.sprintf "https://%s/xrpc/network.bsky.jetstream.listSegments" host
    in
    if qs = "" then base else base ^ "?" ^ qs

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

  let parse_archive_segment json : archive_segment =
    {
      name = string_member json "name";
      index =
        (match Yojson.Safe.Util.member "index" json with `Int n -> n | _ -> 0);
      checksum = string_member json "checksum";
      size_bytes =
        (match Yojson.Safe.Util.member "sizeBytes" json with
        | `Int n -> Some (Int64.of_int n)
        | `Intlit s -> Some (Int64.of_string s)
        | _ -> None);
      event_count =
        (match Yojson.Safe.Util.member "eventCount" json with
        | `Int n -> Some n
        | _ -> None);
      min_seq = int64_member json "minSeq";
      max_seq = int64_member json "maxSeq";
      min_witnessed_at =
        (match int64_member json "minWitnessedAt" with
        | 0L -> None
        | n -> Some n);
      max_witnessed_at =
        (match int64_member json "maxWitnessedAt" with
        | 0L -> None
        | n -> Some n);
    }

  let parse_list_segments json : list_segments =
    {
      cursor = string_opt json "cursor";
      segments =
        (match Yojson.Safe.Util.member "segments" json with
        | `List xs -> List.map parse_archive_segment xs
        | _ -> []);
    }

  (* Official replay loop: pin sealedTipSeq, page while plannedThroughSeq < S. *)
  let plan_needs_next (p : snapshot_plan) : bool =
    Int64.compare p.planned_through_seq p.sealed_tip_seq < 0

  let next_plan_window (p : snapshot_plan) : (int64 * int64) option =
    if plan_needs_next p then Some (p.planned_through_seq, p.sealed_tip_seq)
    else None

  let download_jobs (p : snapshot_plan) : download_job list =
    List.map
      (fun (s : snapshot_segment) ->
        if s.mode = "blocks" && s.blocks <> [] then
          Blocks { name = s.name; checksum = s.checksum; ranges = s.blocks }
        else Segment { name = s.name; checksum = s.checksum })
      p.segments

  let cutover_cursor (p : snapshot_plan) : cursor = Seq p.sealed_tip_seq

  let cutover_filter ?(filter = empty_filter) (p : snapshot_plan) : filter =
    { filter with cursor = Some (cutover_cursor p) }

  let subscribe_url_after_plan ?host ?(filter = empty_filter)
      (p : snapshot_plan) =
    subscribe_url ?host ~filter:(cutover_filter ~filter p) ()

  (* Range resume for getSegment after a mid-download 429. *)
  let range_header ~first ?last () : string * string =
    let spec =
      match last with
      | Some n -> Printf.sprintf "bytes=%d-%d" first n
      | None -> Printf.sprintf "bytes=%d-" first
    in
    ("Range", spec)

  let fold_removes_records (ev : event) : bool =
    match ev with
    | `Account a -> (not a.active) && a.status = Some "deleted"
    | `Sync _ -> true
    | `Commit c -> c.operation = "delete"
    | `Identity _ | `Info _ | `Unknown _ -> false

  (* Live archive HTTP. Public hosts gate this; pass [token] only if the
     operator already has one. This library never invents a token. Live tail
     ([subscribe]) stays unauthenticated. *)
  exception Snapshot_gated of int * string
  exception Snapshot_http of int * string
  exception Snapshot_rate_limited of int * string

  type snapshot_fetch =
    [ `Plan of snapshot_plan | `Bytes of string | `Gated of int * string ]

  let snapshot_headers ?token ?range () =
    let pairs =
      Cohttp_client.Cohttp_client.application_json_setting_tuple
      ::
      (match token with
      | Some t when t <> "" -> [ ("Authorization", "Bearer " ^ t) ]
      | _ -> [])
      @ match range with Some (k, v) -> [ (k, v) ] | None -> []
    in
    Cohttp_client.Cohttp_client.create_headers_from_pairs pairs

  let classify_snapshot_status code body =
    if code = 401 || code = 403 then raise (Snapshot_gated (code, body))
    else if code = 429 then raise (Snapshot_rate_limited (code, body))
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

  let try_get_segment ?host ?token ?range ~name () : string =
    let url = get_segment_url ?host ~name () in
    let headers = snapshot_headers ?token ?range () in
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

  let try_list_segments ?host ?token ?cursor () : list_segments =
    let url = list_segments_url ?host ?cursor () in
    let headers = snapshot_headers ?token () in
    let code, body =
      Lwt_main.run (Cohttp_client.Cohttp_client.get_with_status url headers)
    in
    parse_list_segments
      (Yojson.Safe.from_string (classify_snapshot_status code body))

  let try_plan_backfill ?host ?token ?kinds ?dids ?collections ?after_seq
      ?before_seq () : snapshot_plan =
    let url = plan_backfill_url ?host () in
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

  (* Sealed-segment columnar format (.jss v1). Public spec:
     https://tangled.org/zat.dev/stream/blob/main/docs/jss-format-v1.md
     (condensed from bluesky-social/jetstream segment/*.go).

     Decode only. No archive token and no invented credentials. zstd frames
     may be unwrapped via an injected [decompress] callback; callers can pass
     [decompress_zstd] for the live dict-zstd dictionary. *)
  module Jss = struct
    exception Error of string

    let fail msg = raise (Error msg)
    let magic = "jss0"
    let header_size = 256
    let block_index_entry_size = 52
    let default_events_per_block = 4096
    let max_block_events = 1 lsl 18
    let max_block_count = 1 lsl 20
    let max_collection_count = 1 lsl 20
    let max_decompressed_block = 1 lsl 30
    let max_did_len = 65535
    let max_short_len = 255

    type header = {
      checksum : int64;
      version : int;
      block_count : int;
      event_count : int;
      unique_did_count : int;
      min_seq : int64;
      max_seq : int64;
      min_witnessed_at : int64;
      max_witnessed_at : int64;
      footer_offset : int64;
      did_bloom_offset : int64;
      block_did_bloom_offset : int64;
      collection_index_offset : int64;
      block_index_offset : int64;
      sealed : bool;
    }

    type block_index_entry = {
      offset : int64;
      compressed_size : int;
      uncompressed_size : int;
      event_count : int;
      min_seq : int64;
      max_seq : int64;
      min_witnessed_at : int64;
      max_witnessed_at : int64;
    }

    type row_kind =
      | Create
      | Update
      | Delete
      | Identity
      | Account
      | Sync
      | Create_resync

    type row = {
      seq : int64;
      witnessed_at : int64;
      indexed_at : int64;
      kind : row_kind;
      collection : string;
      did : string;
      rkey : string;
      rev : string;
      payload : string;
    }

    let require_len s need what =
      if String.length s < need then
        fail
          (Printf.sprintf "jss %s: need %d bytes, got %d" what need
             (String.length s))

    let u8 s i = Char.code s.[i]

    let u16_le s i =
      require_len s (i + 2) "u16";
      u8 s i lor (u8 s (i + 1) lsl 8)

    let u32_le s i =
      require_len s (i + 4) "u32";
      Int64.logor
        (Int64.of_int (u8 s i))
        (Int64.logor
           (Int64.shift_left (Int64.of_int (u8 s (i + 1))) 8)
           (Int64.logor
              (Int64.shift_left (Int64.of_int (u8 s (i + 2))) 16)
              (Int64.shift_left (Int64.of_int (u8 s (i + 3))) 24)))

    let u32_le_int s i = Int64.to_int (u32_le s i)

    let u64_le s i =
      require_len s (i + 8) "u64";
      Int64.logor (u32_le s i) (Int64.shift_left (u32_le s (i + 4)) 32)

    let i64_le s i = u64_le s i
    let put_u8 buf n = Buffer.add_char buf (Char.chr (n land 0xff))

    let put_u16_le buf n =
      put_u8 buf n;
      put_u8 buf (n lsr 8)

    let put_u32_le buf n =
      let n = Int64.to_int (Int64.logand n 0xFFFFFFFFL) in
      put_u8 buf n;
      put_u8 buf (n lsr 8);
      put_u8 buf (n lsr 16);
      put_u8 buf (n lsr 24)

    let put_u64_le buf n =
      put_u32_le buf n;
      put_u32_le buf (Int64.shift_right_logical n 32)

    let kind_of_int = function
      | 1 -> Create
      | 2 -> Update
      | 3 -> Delete
      | 4 -> Identity
      | 5 -> Account
      | 6 -> Sync
      | 7 -> Create_resync
      | n -> fail (Printf.sprintf "jss unknown kind %d" n)

    let int_of_kind = function
      | Create -> 1
      | Update -> 2
      | Delete -> 3
      | Identity -> 4
      | Account -> 5
      | Sync -> 6
      | Create_resync -> 7

    let parse_header (bytes : string) : header =
      require_len bytes header_size "header";
      if String.sub bytes 0 4 <> magic then fail "jss magic is not jss0";
      let checksum = u64_le bytes 4 in
      let version = u16_le bytes 12 in
      if version <> 1 then
        fail (Printf.sprintf "jss unsupported version %d" version);
      let block_count = u32_le_int bytes 14 in
      let event_count = u32_le_int bytes 18 in
      if block_count > max_block_count then
        fail "jss block_count exceeds sanity cap";
      {
        checksum;
        version;
        block_count;
        event_count;
        unique_did_count = u32_le_int bytes 22;
        min_seq = u64_le bytes 26;
        max_seq = u64_le bytes 34;
        min_witnessed_at = i64_le bytes 42;
        max_witnessed_at = i64_le bytes 50;
        footer_offset = u64_le bytes 58;
        did_bloom_offset = u64_le bytes 66;
        block_did_bloom_offset = u64_le bytes 74;
        collection_index_offset = u64_le bytes 82;
        block_index_offset = u64_le bytes 90;
        sealed = checksum <> 0L;
      }

    let encode_header (h : header) : string =
      let buf = Buffer.create header_size in
      Buffer.add_string buf magic;
      put_u64_le buf h.checksum;
      put_u16_le buf h.version;
      put_u32_le buf (Int64.of_int h.block_count);
      put_u32_le buf (Int64.of_int h.event_count);
      put_u32_le buf (Int64.of_int h.unique_did_count);
      put_u64_le buf h.min_seq;
      put_u64_le buf h.max_seq;
      put_u64_le buf h.min_witnessed_at;
      put_u64_le buf h.max_witnessed_at;
      put_u64_le buf h.footer_offset;
      put_u64_le buf h.did_bloom_offset;
      put_u64_le buf h.block_did_bloom_offset;
      put_u64_le buf h.collection_index_offset;
      put_u64_le buf h.block_index_offset;
      Buffer.add_bytes buf (Bytes.make 158 '\x00');
      Buffer.contents buf

    let parse_block_index_entry (bytes : string) (off : int) : block_index_entry
        =
      require_len bytes (off + block_index_entry_size) "block index";
      {
        offset = u64_le bytes off;
        compressed_size = u32_le_int bytes (off + 8);
        uncompressed_size = u32_le_int bytes (off + 12);
        event_count = u32_le_int bytes (off + 16);
        min_seq = u64_le bytes (off + 20);
        max_seq = u64_le bytes (off + 28);
        min_witnessed_at = i64_le bytes (off + 36);
        max_witnessed_at = i64_le bytes (off + 44);
      }

    let parse_block_index (bytes : string) (h : header) : block_index_entry list
        =
      if not h.sealed then []
      else
        let off = Int64.to_int h.block_index_offset in
        let rec loop i acc =
          if i >= h.block_count then List.rev acc
          else
            loop (i + 1)
              (parse_block_index_entry bytes (off + (i * block_index_entry_size))
              :: acc)
        in
        loop 0 []

    let slice_by_lens blob lens max_len what =
      let rec loop off acc = function
        | [] ->
            if off <> String.length blob then
              fail (Printf.sprintf "jss %s blob trailing bytes" what);
            List.rev acc
        | n :: rest ->
            if n > max_len then
              fail (Printf.sprintf "jss %s length %d exceeds max" what n);
            if off + n > String.length blob then
              fail (Printf.sprintf "jss %s blob truncated" what);
            loop (off + n) (String.sub blob off n :: acc) rest
      in
      loop 0 [] lens

    let decode_columnar (body : string) : row list =
      require_len body 4 "columnar event_count";
      let n = u32_le_int body 0 in
      if n > max_block_events then fail "jss event_count exceeds sanity cap";
      if n = 0 then
        if String.length body = 4 then []
        else fail "jss empty block has trailing bytes"
      else
        let off = ref 4 in
        let take_arr size read =
          let items = List.init n (fun i -> read body (!off + (i * size))) in
          off := !off + (n * size);
          items
        in
        let seqs = take_arr 8 u64_le in
        let witnessed = take_arr 8 i64_le in
        let indexed = take_arr 8 i64_le in
        let kinds = take_arr 1 (fun s i -> kind_of_int (u8 s i)) in
        let coll_lens = take_arr 1 u8 in
        let did_lens = take_arr 2 u16_le in
        let rkey_lens = take_arr 1 u8 in
        let rev_lens = take_arr 1 u8 in
        let event_lens =
          take_arr 4 (fun s i ->
              let n = u32_le_int s i in
              if n < 0 then fail "jss event_len overflow";
              n)
        in
        let rest = String.sub body !off (String.length body - !off) in
        let take_blob lens max_len what rem =
          let total = List.fold_left ( + ) 0 lens in
          if String.length rem < total then
            fail (Printf.sprintf "jss %s blob truncated" what);
          let blob = String.sub rem 0 total in
          let rem = String.sub rem total (String.length rem - total) in
          (slice_by_lens blob lens max_len what, rem)
        in
        let cols, rest = take_blob coll_lens max_short_len "collection" rest in
        let dids, rest = take_blob did_lens max_did_len "did" rest in
        let rkeys, rest = take_blob rkey_lens max_short_len "rkey" rest in
        let revs, rest = take_blob rev_lens max_short_len "rev" rest in
        let payloads, rest =
          take_blob event_lens max_decompressed_block "payload" rest
        in
        if rest <> "" then fail "jss columnar trailing bytes";
        let rec zip acc s w i k c d r v p =
          match (s, w, i, k, c, d, r, v, p) with
          | [], [], [], [], [], [], [], [], [] -> List.rev acc
          | ( s :: ss,
              w :: ws,
              i :: is,
              k :: ks,
              c :: cs,
              d :: ds,
              r :: rs,
              v :: vs,
              p :: ps ) ->
              zip
                ({
                   seq = s;
                   witnessed_at = w;
                   indexed_at = i;
                   kind = k;
                   collection = c;
                   did = d;
                   rkey = r;
                   rev = v;
                   payload = p;
                 }
                :: acc)
                ss ws is ks cs ds rs vs ps
          | _ -> fail "jss columnar column length mismatch"
        in
        zip [] seqs witnessed indexed kinds cols dids rkeys revs payloads

    let encode_columnar (rows : row list) : string =
      let n = List.length rows in
      let buf = Buffer.create 64 in
      put_u32_le buf (Int64.of_int n);
      List.iter (fun r -> put_u64_le buf r.seq) rows;
      List.iter (fun r -> put_u64_le buf r.witnessed_at) rows;
      List.iter (fun r -> put_u64_le buf r.indexed_at) rows;
      List.iter (fun r -> put_u8 buf (int_of_kind r.kind)) rows;
      List.iter (fun r -> put_u8 buf (String.length r.collection)) rows;
      List.iter (fun r -> put_u16_le buf (String.length r.did)) rows;
      List.iter (fun r -> put_u8 buf (String.length r.rkey)) rows;
      List.iter (fun r -> put_u8 buf (String.length r.rev)) rows;
      List.iter
        (fun r -> put_u32_le buf (Int64.of_int (String.length r.payload)))
        rows;
      List.iter (fun r -> Buffer.add_string buf r.collection) rows;
      List.iter (fun r -> Buffer.add_string buf r.did) rows;
      List.iter (fun r -> Buffer.add_string buf r.rkey) rows;
      List.iter (fun r -> Buffer.add_string buf r.rev) rows;
      List.iter (fun r -> Buffer.add_string buf r.payload) rows;
      Buffer.contents buf

    let time_of_us us =
      let secs = Int64.to_float (Int64.div us 1_000_000L) in
      match Ptime.of_float_s secs with
      | None -> Printf.sprintf "%Ld" us
      | Some t -> Ptime.to_rfc3339 ~frac_s:6 t

    let row_to_event (r : row) : event =
      match r.kind with
      | Create | Update | Delete | Create_resync ->
          let operation =
            match r.kind with
            | Update -> "update"
            | Delete -> "delete"
            | _ -> "create"
          in
          `Commit
            {
              did = r.did;
              seq = r.seq;
              time = time_of_us r.witnessed_at;
              operation;
              collection = r.collection;
              rkey = r.rkey;
              rev = r.rev;
              cid = None;
              record = None;
            }
      | Identity ->
          `Identity
            {
              did = r.did;
              seq = r.seq;
              time = time_of_us r.witnessed_at;
              handle = None;
            }
      | Account ->
          `Account
            {
              did = r.did;
              seq = r.seq;
              time = time_of_us r.witnessed_at;
              active = true;
              status = None;
            }
      | Sync ->
          `Sync
            {
              did = r.did;
              seq = r.seq;
              time = time_of_us r.witnessed_at;
              rev = r.rev;
            }

    (* Sequential frame walk. Each frame is `block_len u64` + `block_len`
       compressed bytes. Pass [decompress] to unwrap zstd; [decompress_zstd]
       is the built-in dict-zstd decoder. *)
    let walk_frames ?(decompress : (string -> string) option) (bytes : string) :
        row list =
      let h = parse_header bytes in
      let rec loop i acc =
        if i >= String.length bytes then List.rev acc
        else if
          h.sealed && Int64.of_int i >= h.footer_offset && h.footer_offset <> 0L
        then List.rev acc
        else if i + 8 > String.length bytes then List.rev acc
        else
          let len64 = u64_le bytes i in
          let len = Int64.to_int len64 in
          if len < 0 || i + 8 + len > String.length bytes then
            fail "jss truncated block frame";
          let frame = String.sub bytes (i + 8) len in
          let body =
            match decompress with Some f -> f frame | None -> frame
          in
          if String.length body > max_decompressed_block then
            fail "jss decompressed block exceeds 1 GiB";
          loop (i + 8 + len) (List.rev_append (decode_columnar body) acc)
      in
      loop header_size []
  end
end
