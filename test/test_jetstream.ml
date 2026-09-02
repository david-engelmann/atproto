open OUnit2
open Atproto.Jetstream

let v2_commit_json =
  `Assoc
    [
      ("$type", `String "message");
      ( "payload",
        `Assoc
          [
            ("$type", `String "network.bsky.jetstream.subscribeEvents#commit");
            ("did", `String "did:plc:7e6kocyzb77xkncplrkoojej");
            ("seq", `Intlit "24664288881");
            ("time", `String "2026-08-13T06:47:43.959305Z");
            ("operation", `String "create");
            ("collection", `String "app.bsky.feed.like");
            ("rkey", `String "3msx2efqdxs27");
            ("rev", `String "3msx2efqjtc27");
            ( "cid",
              `String
                "bafyreigwnxqttkhzha2ig4io6wwht3qiugtor4ruglceyfdbnyq53a55fe" );
            ( "record",
              `Assoc
                [
                  ("$type", `String "app.bsky.feed.like");
                  ("createdAt", `String "2026-08-13T06:47:44.859Z");
                ] );
          ] );
    ]

let v1_commit_json =
  `Assoc
    [
      ("did", `String "did:plc:7e6kocyzb77xkncplrkoojej");
      ("time_us", `Intlit "1724084535744408");
      ("kind", `String "commit");
      ( "commit",
        `Assoc
          [
            ("rev", `String "3jzfcijpj2z2a");
            ("operation", `String "create");
            ("collection", `String "app.bsky.feed.post");
            ("rkey", `String "3jzfcijpj2z2a");
            ("record", `Assoc [ ("text", `String "hi") ]);
            ("cid", `String "bafyreihdummy000000000000000000000000000000000");
          ] );
    ]

let test_parse_v2_commit _ =
  match Jetstream.parse_event v2_commit_json with
  | `Commit c ->
      OUnit2.assert_equal
        ~printer:(fun x -> x)
        "app.bsky.feed.like" c.collection;
      OUnit2.assert_equal ~printer:(fun x -> x) "create" c.operation;
      OUnit2.assert_equal ~printer:Int64.to_string 24664288881L c.seq;
      OUnit2.assert_equal
        ~printer:(fun x -> x)
        "at://did:plc:7e6kocyzb77xkncplrkoojej/app.bsky.feed.like/3msx2efqdxs27"
        (Jetstream.record_uri c)
  | _ -> OUnit2.assert_failure "expected v2 commit"

let test_parse_v1_commit _ =
  match Jetstream.parse_event v1_commit_json with
  | `Commit c ->
      OUnit2.assert_equal
        ~printer:(fun x -> x)
        "app.bsky.feed.post" c.collection;
      OUnit2.assert_equal ~printer:Int64.to_string 1724084535744408L c.seq
  | _ -> OUnit2.assert_failure "expected v1 commit"

let test_parse_v2_identity _ =
  let json =
    `Assoc
      [
        ("$type", `String "message");
        ( "payload",
          `Assoc
            [
              ( "$type",
                `String "network.bsky.jetstream.subscribeEvents#identity" );
              ("did", `String "did:plc:abc123xyz0001112223333");
              ("seq", `Int 9);
              ("time", `String "2026-01-01T00:00:00.000000Z");
              ( "identity",
                `Assoc
                  [
                    ("did", `String "did:plc:abc123xyz0001112223333");
                    ("handle", `String "jay.bsky.team");
                    ("seq", `Int 1);
                    ("time", `String "2026-01-01T00:00:00.000Z");
                  ] );
            ] );
      ]
  in
  match Jetstream.parse_event json with
  | `Identity i ->
      OUnit2.assert_equal (Some "jay.bsky.team") i.handle;
      OUnit2.assert_equal ~printer:Int64.to_string 9L i.seq
  | _ -> OUnit2.assert_failure "expected identity"

let test_subscribe_url_filters _ =
  let url =
    Jetstream.subscribe_url
      ~filter:
        {
          Jetstream.collections = [ "app.bsky.feed.post"; "app.bsky.feed.like" ];
          dids = [ "did:plc:abc123xyz0001112223333" ];
          kinds = [ Jetstream.Commit ];
          cursor = Some (Jetstream.Seq 12345L);
          max_message_size_bytes = None;
        }
      ()
  in
  let has needle =
    let rec contains i =
      i + String.length needle <= String.length url
      && (String.sub url i (String.length needle) = needle || contains (i + 1))
    in
    contains 0
  in
  OUnit2.assert_bool "v2 host" (has "jetstream.us-west.bsky.network");
  OUnit2.assert_bool "nsid" (has "network.bsky.jetstream.subscribeEvents");
  OUnit2.assert_bool "collections" (has "collections=");
  OUnit2.assert_bool "kinds" (has "kinds=commit");
  OUnit2.assert_bool "cursor" (has "cursor=12345")

let test_v1_url _ =
  let url =
    Jetstream.subscribe_url ~host:Jetstream.v1_west_host ~version:Jetstream.V1
      ~filter:
        {
          Jetstream.empty_filter with
          collections = [ "app.bsky.feed.post" ];
          cursor = Some (Jetstream.Time_us 1724084535744408L);
        }
      ()
  in
  OUnit2.assert_bool "v1 path"
    (let needle = "/subscribe" in
     let rec contains i =
       i + String.length needle <= String.length url
       && (String.sub url i (String.length needle) = needle || contains (i + 1))
     in
     contains 0);
  OUnit2.assert_bool "wantedCollections"
    (let needle = "wantedCollections=" in
     let rec contains i =
       i + String.length needle <= String.length url
       && (String.sub url i (String.length needle) = needle || contains (i + 1))
     in
     contains 0)

let test_cursor_magnitude _ =
  OUnit2.assert_equal (Jetstream.Seq 99L) (Jetstream.cursor_of_int64 99L);
  match Jetstream.cursor_of_int64 1724084535744408L with
  | Jetstream.Time_us n ->
      OUnit2.assert_equal ~printer:Int64.to_string 1724084535744408L n
  | Jetstream.Seq _ -> OUnit2.assert_failure "expected time_us cursor"

let test_collection_wildcard_allowed _ =
  Jetstream.validate_filter
    {
      Jetstream.empty_filter with
      collections = [ "app.bsky.feed.*" ];
      kinds = [ Jetstream.Commit ];
    }

let test_filter_limits _ =
  OUnit2.assert_raises (Jetstream.Invalid_filter "collections cap is 100")
    (fun () ->
      Jetstream.validate_filter
        {
          Jetstream.empty_filter with
          collections =
            List.init 101 (fun i -> "app.bsky.feed.n" ^ string_of_int i);
        });
  OUnit2.assert_raises
    (Jetstream.Invalid_filter
       "collections filter requires kinds to include commit (or omit kinds)")
    (fun () ->
      Jetstream.validate_filter
        {
          Jetstream.empty_filter with
          collections = [ "app.bsky.feed.post" ];
          kinds = [ Jetstream.Identity ];
        })

let test_dedupe _ =
  let seen = Jetstream.create_seen ~cap:2 () in
  let ev = Jetstream.parse_event v2_commit_json in
  OUnit2.assert_bool "first" (not (Jetstream.is_duplicate seen ev));
  Jetstream.remember seen ev;
  OUnit2.assert_bool "dup" (Jetstream.is_duplicate seen ev)

let test_reconnect_cursor _ =
  let ev = Jetstream.parse_event v2_commit_json in
  match Jetstream.seq_of ev with
  | Some s ->
      let f = Jetstream.with_cursor Jetstream.empty_filter (Jetstream.Seq s) in
      OUnit2.assert_equal (Some (Jetstream.Seq s)) f.cursor
  | None -> OUnit2.assert_failure "commit should have seq"

let test_plan_snapshot _ =
  let body =
    Jetstream.plan_snapshot_body ~kinds:[ "commit" ]
      ~collections:[ "app.bsky.feed.post" ] ~after_seq:10L ()
  in
  let open Yojson.Safe.Util in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "commit"
    (body |> member "kinds" |> to_list |> List.hd |> to_string);
  let plan =
    Jetstream.parse_snapshot_plan
      (`Assoc
        [
          ("plannedThroughSeq", `Int 20);
          ("sealedTipSeq", `Int 20);
          ( "segments",
            `List
              [
                `Assoc
                  [
                    ("name", `String "seg-0");
                    ("index", `Int 0);
                    ("checksum", `String "0123456789abcdef");
                    ("minSeq", `Int 1);
                    ("maxSeq", `Int 20);
                    ("mode", `String "segment");
                  ];
              ] );
          ( "stats",
            `Assoc
              [
                ("segmentsExamined", `Int 1);
                ("segmentsMatched", `Int 1);
                ("blocksMatched", `Int 0);
                ("entries", `Int 1);
              ] );
        ])
  in
  OUnit2.assert_equal ~printer:Int64.to_string 20L plan.sealed_tip_seq;
  OUnit2.assert_equal 1 (List.length plan.segments);
  OUnit2.assert_bool "plan URL"
    (let u = Jetstream.plan_snapshot_url () in
     let needle = "planSnapshot" in
     let rec contains i =
       i + String.length needle <= String.length u
       && (String.sub u i (String.length needle) = needle || contains (i + 1))
     in
     contains 0)

let test_replay_planner _ =
  let page =
    Jetstream.parse_snapshot_plan
      (`Assoc
        [
          ("plannedThroughSeq", `Int 10);
          ("sealedTipSeq", `Int 20);
          ( "segments",
            `List
              [
                `Assoc
                  [
                    ("name", `String "seg_0000000000.jss");
                    ("index", `Int 0);
                    ("checksum", `String "0c9577a8002d2b24");
                    ("minSeq", `Int 1);
                    ("maxSeq", `Int 10);
                    ("mode", `String "blocks");
                    ( "blocks",
                      `List [ `Assoc [ ("first", `Int 7); ("last", `Int 9) ] ]
                    );
                  ];
              ] );
          ("stats", `Assoc [ ("entries", `Int 1) ]);
        ])
  in
  OUnit2.assert_bool "needs next page" (Jetstream.plan_needs_next page);
  (match Jetstream.next_plan_window page with
  | Some (after, before) ->
      OUnit2.assert_equal ~printer:Int64.to_string 10L after;
      OUnit2.assert_equal ~printer:Int64.to_string 20L before
  | None -> OUnit2.assert_failure "expected next window");
  (match Jetstream.download_jobs page with
  | [ Jetstream.Blocks { name; ranges; _ } ] ->
      OUnit2.assert_equal ~printer:(fun x -> x) "seg_0000000000.jss" name;
      OUnit2.assert_equal 7 (List.hd ranges).first
  | _ -> OUnit2.assert_failure "expected block-range job");
  let done_ = { page with planned_through_seq = 20L } in
  OUnit2.assert_bool "complete" (not (Jetstream.plan_needs_next done_));
  OUnit2.assert_equal None (Jetstream.next_plan_window done_);
  let url = Jetstream.subscribe_url_after_plan done_ in
  OUnit2.assert_bool "cutover cursor"
    (let needle = "cursor=20" in
     let rec contains i =
       i + String.length needle <= String.length url
       && (String.sub url i (String.length needle) = needle || contains (i + 1))
     in
     contains 0);
  let h, v = Jetstream.range_header ~first:1024 () in
  OUnit2.assert_equal "Range" h;
  OUnit2.assert_equal ~printer:(fun x -> x) "bytes=1024-" v;
  let listed =
    Jetstream.parse_list_segments
      (`Assoc
        [
          ( "segments",
            `List
              [
                `Assoc
                  [
                    ("name", `String "seg_0000000000.jss");
                    ("index", `Int 0);
                    ("sizeBytes", `Int 193462065);
                    ("checksum", `String "0c9577a8002d2b24");
                    ("eventCount", `Int 2569479);
                    ("minSeq", `Int 1);
                    ("maxSeq", `Int 2569835);
                    ("minWitnessedAt", `Intlit "1785262575375952");
                    ("maxWitnessedAt", `Intlit "1785262678113580");
                  ];
              ] );
        ])
  in
  OUnit2.assert_equal 1 (List.length listed.segments);
  OUnit2.assert_equal (Some 2569479) (List.hd listed.segments).event_count;
  OUnit2.assert_bool "backfill URL"
    (let u = Jetstream.plan_backfill_url () in
     let needle = "planBackfill" in
     let rec contains i =
       i + String.length needle <= String.length u
       && (String.sub u i (String.length needle) = needle || contains (i + 1))
     in
     contains 0);
  OUnit2.assert_bool "delete folds"
    (Jetstream.fold_removes_records
       (Jetstream.parse_event
          (`Assoc
            [
              ("kind", `String "commit");
              ("did", `String "did:plc:7e6kocyzb77xkncplrkoojej");
              ("time_us", `Int 1);
              ( "commit",
                `Assoc
                  [
                    ("operation", `String "delete");
                    ("collection", `String "app.bsky.feed.post");
                    ("rkey", `String "3jzfcijpj2z2a");
                    ("rev", `String "3jzfcijpj2z2a");
                  ] );
            ])))

let test_snapshot_gated_live _ =
  let old =
    Sys.signal Sys.sigalrm (Sys.Signal_handle (fun _ -> failwith "timeout"))
  in
  ignore (Unix.alarm 20);
  Fun.protect
    ~finally:(fun () ->
      ignore (Unix.alarm 0);
      Sys.set_signal Sys.sigalrm old)
    (fun () ->
      try
        let _ = Jetstream.try_plan_snapshot () in
        OUnit2.assert_bool "ungated archive returned a plan" true
      with
      | Jetstream.Snapshot_gated (code, _) ->
          OUnit2.assert_bool "gated without inventing a token"
            (code = 401 || code = 403)
      | Jetstream.Snapshot_http (code, _) ->
          skip_if true (Printf.sprintf "planSnapshot HTTP %d" code)
      | exn -> skip_if true ("planSnapshot skipped: " ^ Printexc.to_string exn))

let live_post_filter =
  {
    Jetstream.empty_filter with
    collections = [ "app.bsky.feed.post" ];
    kinds = [ Jetstream.Commit ];
  }

let assert_decoded_event label ev =
  match ev with
  | `Commit _ | `Identity _ | `Account _ | `Sync _ | `Info _ | `Unknown _ ->
      OUnit2.assert_bool label true

let skip_or_fail_live label = function
  | Atproto.Websocket.Websocket.Subprotocol_error msg ->
      OUnit2.assert_failure
        (label ^ ": 101 omitted or mismatched xrpc.v1.json echo: " ^ msg)
  | Atproto.Websocket.Websocket.Handshake_error (code, body) ->
      skip_if true
        (Printf.sprintf "%s skipped: handshake %d %s" label code body)
  | Failure msg as exn ->
      let needle = "Sec-WebSocket-Protocol" in
      let rec contains i =
        i + String.length needle <= String.length msg
        && (String.sub msg i (String.length needle) = needle || contains (i + 1))
      in
      if contains 0 then
        OUnit2.assert_failure
          (label ^ ": 101 omitted or mismatched xrpc.v1.json echo: " ^ msg)
      else skip_if true (label ^ " skipped: " ^ Printexc.to_string exn)
  | exn -> skip_if true (label ^ " skipped: " ^ Printexc.to_string exn)

let with_alarm seconds f =
  let old =
    Sys.signal Sys.sigalrm (Sys.Signal_handle (fun _ -> failwith "timeout"))
  in
  ignore (Unix.alarm seconds);
  Fun.protect
    ~finally:(fun () ->
      ignore (Unix.alarm 0);
      Sys.set_signal Sys.sigalrm old)
    f

let test_subscribe_live _ =
  with_alarm 20 (fun () ->
      try
        let ev = Jetstream.subscribe_one ~filter:live_post_filter () in
        assert_decoded_event "decoded a Jetstream frame" ev
      with exn -> skip_or_fail_live "jetstream" exn)

let test_subscribe_one_subprotocol_live _ =
  with_alarm 20 (fun () ->
      let headers = Jetstream.subscribe_extra_headers () in
      OUnit2.assert_equal [ ("Sec-WebSocket-Protocol", "xrpc.v1.json") ] headers;
      try
        let ev = Jetstream.subscribe_one ~filter:live_post_filter () in
        assert_decoded_event
          "decoded a Jetstream v2 event with xrpc.v1.json negotiated" ev
      with exn -> skip_or_fail_live "jetstream subprotocol" exn)

let test_subscribe_extra_headers _ =
  let v2 = Jetstream.subscribe_extra_headers () in
  OUnit2.assert_equal
    [ ("Sec-WebSocket-Protocol", Jetstream.xrpc_v1_json_subprotocol) ]
    v2;
  let v2_zstd = Jetstream.subscribe_extra_headers ~compress:true () in
  OUnit2.assert_equal
    [ ("Sec-WebSocket-Protocol", Jetstream.xrpc_v1_json_subprotocol) ]
    v2_zstd;
  let v1 = Jetstream.subscribe_extra_headers ~version:Jetstream.V1 () in
  OUnit2.assert_equal [] v1;
  let v1_zstd =
    Jetstream.subscribe_extra_headers ~version:Jetstream.V1 ~compress:true ()
  in
  OUnit2.assert_equal [ ("Socket-Encoding", "zstd") ] v1_zstd

let test_subscribe_url_compress _ =
  let has url needle =
    let rec contains i =
      i + String.length needle <= String.length url
      && (String.sub url i (String.length needle) = needle || contains (i + 1))
    in
    contains 0
  in
  let v2 =
    Jetstream.subscribe_url ~compress:true ~zstd_dictionary_id:20260811 ()
  in
  OUnit2.assert_bool "v2 zstdDictionary" (has v2 "zstdDictionary=20260811");
  OUnit2.assert_bool "v2 omits v1 compress=" (not (has v2 "compress="));
  let v1 =
    Jetstream.subscribe_url ~host:Jetstream.v1_west_host ~version:Jetstream.V1
      ~compress:true ()
  in
  OUnit2.assert_bool "v1 compress=true" (has v1 "compress=true");
  OUnit2.assert_bool "v1 omits zstdDictionary" (not (has v1 "zstdDictionary="));
  let plain = Jetstream.subscribe_url () in
  OUnit2.assert_bool "default uncompressed"
    (not (has plain "compress=" || has plain "zstdDictionary="))

let test_dict_zstd_roundtrip _ =
  OUnit2.assert_equal (Some 20260811)
    (Jetstream.zstd_dictionary_id Jetstream.embedded_zstd_dictionary);
  let json = Yojson.Safe.to_string v2_commit_json in
  let frame = Jetstream.compress_zstd json in
  OUnit2.assert_equal (Some 20260811) (Jetstream.zstd_frame_dict_id frame);
  let got = Jetstream.decompress_zstd frame in
  (match Jetstream.parse_frame got with
  | `Commit c ->
      OUnit2.assert_equal
        ~printer:(fun x -> x)
        "app.bsky.feed.like" c.collection
  | _ -> OUnit2.assert_failure "expected commit after dict-zstd roundtrip");
  let mutated = Bytes.of_string Jetstream.embedded_zstd_dictionary in
  Bytes.set mutated 4 '\x01';
  Bytes.set mutated 5 '\x00';
  Bytes.set mutated 6 '\x00';
  Bytes.set mutated 7 '\x00';
  let wrong = Bytes.to_string mutated in
  OUnit2.assert_equal (Some 1) (Jetstream.zstd_dictionary_id wrong);
  OUnit2.assert_raises
    (Jetstream.Unknown_zstd_dictionary (20260811, Some 1))
    (fun () -> ignore (Jetstream.decompress_zstd ~dictionary:wrong frame))

let test_jss_walk_dict_zstd _ =
  let open Jetstream.Jss in
  let header =
    {
      checksum = 0L;
      version = 1;
      block_count = 1;
      event_count = 1;
      unique_did_count = 1;
      min_seq = 1L;
      max_seq = 1L;
      min_witnessed_at = 1_700_000_000_000_000L;
      max_witnessed_at = 1_700_000_000_000_001L;
      footer_offset = 0L;
      did_bloom_offset = 0L;
      block_did_bloom_offset = 0L;
      collection_index_offset = 0L;
      block_index_offset = 0L;
      sealed = false;
    }
  in
  let encoded = encode_header header in
  let row =
    {
      seq = 7L;
      witnessed_at = 1_700_000_000_000_000L;
      indexed_at = 0L;
      kind = Create;
      collection = "app.bsky.feed.post";
      did = "did:plc:7iza6de2dwap2sbkpav7c6c6";
      rkey = "3jzfcijpj2z2a";
      rev = "3jzfcijpj2z2a";
      payload = "cbor";
    }
  in
  let body = encode_columnar [ row ] in
  let zbody = Jetstream.compress_zstd body in
  let framed =
    let buf = Buffer.create 64 in
    Buffer.add_string buf encoded;
    let len = String.length zbody in
    let len64 = Bytes.create 8 in
    for i = 0 to 7 do
      Bytes.set len64 i (Char.chr ((len lsr (8 * i)) land 0xff))
    done;
    Buffer.add_bytes buf len64;
    Buffer.add_string buf zbody;
    Buffer.contents buf
  in
  let walked = walk_frames ~decompress:Jetstream.decompress_zstd framed in
  OUnit2.assert_equal 1 (List.length walked);
  OUnit2.assert_equal ~printer:Int64.to_string 7L (List.hd walked).seq

let test_get_zstd_dictionary_live _ =
  let old =
    Sys.signal Sys.sigalrm (Sys.Signal_handle (fun _ -> failwith "timeout"))
  in
  ignore (Unix.alarm 20);
  Fun.protect
    ~finally:(fun () ->
      ignore (Unix.alarm 0);
      Sys.set_signal Sys.sigalrm old)
    (fun () ->
      try
        let blob = Jetstream.try_get_zstd_dictionary () in
        match Jetstream.zstd_dictionary_id blob with
        | Some id -> OUnit2.assert_bool "live dict id" (id > 0)
        | None ->
            OUnit2.assert_failure
              "getZstdDictionary returned bytes that are not a zstd dictionary"
      with exn ->
        skip_if true ("getZstdDictionary skipped: " ^ Printexc.to_string exn))

let test_subscribe_one_compress_live _ =
  let old =
    Sys.signal Sys.sigalrm (Sys.Signal_handle (fun _ -> failwith "timeout"))
  in
  ignore (Unix.alarm 20);
  Fun.protect
    ~finally:(fun () ->
      ignore (Unix.alarm 0);
      Sys.set_signal Sys.sigalrm old)
    (fun () ->
      (try ignore (Jetstream.try_get_zstd_dictionary ())
       with exn ->
         skip_if true ("getZstdDictionary skipped: " ^ Printexc.to_string exn));
      try
        let ev =
          Jetstream.subscribe_one ~compress:true
            ~filter:
              {
                Jetstream.empty_filter with
                collections = [ "app.bsky.feed.post" ];
                kinds = [ Jetstream.Commit ];
              }
            ()
        in
        match ev with
        | `Commit _ | `Identity _ | `Account _ | `Sync _ | `Info _ | `Unknown _
          ->
            OUnit2.assert_bool "decoded a compressed Jetstream frame" true
      with
      | Jetstream.Zstd_decode msg ->
          OUnit2.assert_failure ("dict-zstd decode failed: " ^ msg)
      | Jetstream.Unknown_zstd_dictionary (got, have) ->
          OUnit2.assert_failure
            (Printf.sprintf "dict id mismatch got=%d have=%s" got
               (match have with Some n -> string_of_int n | None -> "none"))
      | Yojson.Json_error msg ->
          OUnit2.assert_failure ("compressed frame was not JSON: " ^ msg)
      | Atproto.Websocket.Websocket.Subprotocol_error msg ->
          OUnit2.assert_failure
            ("compressed v2 101 omitted or mismatched xrpc.v1.json echo: " ^ msg)
      | Atproto.Websocket.Websocket.Handshake_error (code, body) -> (
          match Atproto.Error.Error.of_body body with
          | Some e
            when e.error = "InvalidRequest" || e.error = "UnknownZstdDictionary"
            ->
              OUnit2.assert_failure
                (Printf.sprintf "compressed subscribe HTTP %d %s: %s" code
                   e.error e.message)
          | _ ->
              skip_if true
                (Printf.sprintf "jetstream compressed skipped: handshake %d %s"
                   code body))
      | exn ->
          skip_if true
            ("jetstream compressed skipped: " ^ Printexc.to_string exn))

let test_jss_header_and_columnar _ =
  let open Jetstream.Jss in
  let header =
    {
      checksum = 0L;
      version = 1;
      block_count = 1;
      event_count = 1;
      unique_did_count = 1;
      min_seq = 1L;
      max_seq = 1L;
      min_witnessed_at = 1_700_000_000_000_000L;
      max_witnessed_at = 1_700_000_000_000_001L;
      footer_offset = 0L;
      did_bloom_offset = 0L;
      block_did_bloom_offset = 0L;
      collection_index_offset = 0L;
      block_index_offset = 0L;
      sealed = false;
    }
  in
  let encoded = encode_header header in
  OUnit2.assert_equal ~printer:string_of_int 256 (String.length encoded);
  let parsed = parse_header encoded in
  OUnit2.assert_equal 1 parsed.version;
  OUnit2.assert_equal 1 parsed.block_count;
  OUnit2.assert_equal ~printer:Int64.to_string 1L parsed.min_seq;
  OUnit2.assert_bool "unsealed" (not parsed.sealed);
  let row =
    {
      seq = 42L;
      witnessed_at = 1_700_000_000_000_000L;
      indexed_at = 0L;
      kind = Create;
      collection = "app.bsky.feed.post";
      did = "did:plc:7iza6de2dwap2sbkpav7c6c6";
      rkey = "3jzfcijpj2z2a";
      rev = "3jzfcijpj2z2a";
      payload = "cbor";
    }
  in
  let body = encode_columnar [ row ] in
  let decoded = decode_columnar body in
  OUnit2.assert_equal 1 (List.length decoded);
  let got = List.hd decoded in
  OUnit2.assert_equal ~printer:Int64.to_string 42L got.seq;
  OUnit2.assert_equal ~printer:(fun x -> x) "app.bsky.feed.post" got.collection;
  OUnit2.assert_equal ~printer:(fun x -> x) "cbor" got.payload;
  (match row_to_event got with
  | `Commit c ->
      OUnit2.assert_equal ~printer:(fun x -> x) "create" c.operation;
      OUnit2.assert_equal ~printer:(fun x -> x) row.rkey c.rkey
  | _ -> OUnit2.assert_failure "expected commit event");
  let empty = decode_columnar (encode_columnar []) in
  OUnit2.assert_equal 0 (List.length empty);
  let framed =
    let buf = Buffer.create 64 in
    Buffer.add_string buf encoded;
    let len = String.length body in
    let len64 = Bytes.create 8 in
    for i = 0 to 7 do
      Bytes.set len64 i (Char.chr ((len lsr (8 * i)) land 0xff))
    done;
    Buffer.add_bytes buf len64;
    Buffer.add_string buf body;
    Buffer.contents buf
  in
  let walked = walk_frames framed in
  OUnit2.assert_equal 1 (List.length walked);
  OUnit2.assert_equal ~printer:Int64.to_string 42L (List.hd walked).seq

let suite =
  "jetstream"
  >::: [
         "test_parse_v2_commit" >:: test_parse_v2_commit;
         "test_parse_v1_commit" >:: test_parse_v1_commit;
         "test_parse_v2_identity" >:: test_parse_v2_identity;
         "test_subscribe_url_filters" >:: test_subscribe_url_filters;
         "test_v1_url" >:: test_v1_url;
         "test_cursor_magnitude" >:: test_cursor_magnitude;
         "test_collection_wildcard_allowed" >:: test_collection_wildcard_allowed;
         "test_filter_limits" >:: test_filter_limits;
         "test_dedupe" >:: test_dedupe;
         "test_reconnect_cursor" >:: test_reconnect_cursor;
         "test_plan_snapshot" >:: test_plan_snapshot;
         "test_replay_planner" >:: test_replay_planner;
         "test_snapshot_gated_live" >:: test_snapshot_gated_live;
         "test_subscribe_live" >:: test_subscribe_live;
         "test_subscribe_one_subprotocol_live"
         >:: test_subscribe_one_subprotocol_live;
         "test_subscribe_extra_headers" >:: test_subscribe_extra_headers;
         "test_subscribe_url_compress" >:: test_subscribe_url_compress;
         "test_dict_zstd_roundtrip" >:: test_dict_zstd_roundtrip;
         "test_jss_walk_dict_zstd" >:: test_jss_walk_dict_zstd;
         "test_get_zstd_dictionary_live" >:: test_get_zstd_dictionary_live;
         "test_subscribe_one_compress_live" >:: test_subscribe_one_compress_live;
         "test_jss_header_and_columnar" >:: test_jss_header_and_columnar;
       ]

let () = run_test_tt_main suite
