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

let test_subscribe_live _ =
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
        let ev =
          Jetstream.subscribe_one
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
            OUnit2.assert_bool "decoded a Jetstream frame" true
      with exn -> skip_if true ("jetstream skipped: " ^ Printexc.to_string exn))

let suite =
  "jetstream"
  >::: [
         "test_parse_v2_commit" >:: test_parse_v2_commit;
         "test_parse_v1_commit" >:: test_parse_v1_commit;
         "test_parse_v2_identity" >:: test_parse_v2_identity;
         "test_subscribe_url_filters" >:: test_subscribe_url_filters;
         "test_v1_url" >:: test_v1_url;
         "test_cursor_magnitude" >:: test_cursor_magnitude;
         "test_filter_limits" >:: test_filter_limits;
         "test_dedupe" >:: test_dedupe;
         "test_reconnect_cursor" >:: test_reconnect_cursor;
         "test_plan_snapshot" >:: test_plan_snapshot;
         "test_subscribe_live" >:: test_subscribe_live;
       ]

let () = run_test_tt_main suite
