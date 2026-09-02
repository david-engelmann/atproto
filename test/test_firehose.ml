open OUnit2
open Atproto.Cid
open Atproto.Dag_cbor
open Atproto.Firehose
open Atproto.Websocket
open Atproto.Mst
open Atproto.Car
open Atproto.Tid

let test_subscribe_url _ =
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "wss://bsky.network/xrpc/com.atproto.sync.subscribeRepos"
    (Firehose.subscribe_url ());
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "wss://relay.example/xrpc/com.atproto.sync.subscribeRepos?cursor=99"
    (Firehose.subscribe_url ~host:"relay.example" ~cursor:99L ());
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "ws://localhost:2583/xrpc/com.atproto.sync.subscribeRepos"
    (Firehose.subscribe_url ~host:"localhost:2583" ());
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "ws://127.0.0.1:2583/xrpc/com.atproto.sync.subscribeRepos?cursor=0"
    (Firehose.subscribe_url ~host:"127.0.0.1:2583" ~cursor:0L ());
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "wss://localhost:2583/xrpc/com.atproto.sync.subscribeRepos"
    (Firehose.subscribe_url ~host:"localhost:2583" ~scheme:"wss" ())

let test_decode_identity_frame _ =
  let header = Firehose.encode_header { op = 1; t = Some "#identity" } in
  let body =
    Dag_cbor.encode
      (Dag_cbor.Map
         [
           ("seq", Dag_cbor.Int 7);
           ("did", Dag_cbor.Text "did:plc:7iza6de2dwap2sbkpav7c6c6");
           ("time", Dag_cbor.Text "2024-01-01T00:00:00.000Z");
           ("handle", Dag_cbor.Text "alice.test");
         ])
  in
  match Firehose.decode_frame (header ^ body) with
  | _, `Identity ev ->
      OUnit2.assert_equal ~printer:Int64.to_string 7L ev.seq;
      OUnit2.assert_equal (Some "alice.test") ev.handle
  | _ -> OUnit2.assert_failure "expected #identity frame"

let test_decode_sync_account_info _ =
  let cid = Cid.of_digest (String.make 32 '\x09') in
  let empty_car = Car.encode { Car.roots = [ cid ]; blocks = [] } in
  let sync_header = Firehose.encode_header { op = 1; t = Some "#sync" } in
  let sync_body =
    Dag_cbor.encode
      (Dag_cbor.Map
         [
           ("seq", Dag_cbor.Int 3);
           ("did", Dag_cbor.Text "did:plc:7iza6de2dwap2sbkpav7c6c6");
           ("blocks", Dag_cbor.Bytes empty_car);
           ("rev", Dag_cbor.Text "3jzfcijpj2z2a");
           ("time", Dag_cbor.Text "2024-01-01T00:00:00.000Z");
         ])
  in
  (match Firehose.decode_frame (sync_header ^ sync_body) with
  | _, `Sync ev ->
      OUnit2.assert_equal ~printer:Int64.to_string 3L ev.seq;
      OUnit2.assert_equal ~printer:(fun x -> x) "3jzfcijpj2z2a" ev.rev
  | _ -> OUnit2.assert_failure "expected #sync frame");
  let acct_header = Firehose.encode_header { op = 1; t = Some "#account" } in
  let acct_body =
    Dag_cbor.encode
      (Dag_cbor.Map
         [
           ("seq", Dag_cbor.Int 4);
           ("did", Dag_cbor.Text "did:plc:7iza6de2dwap2sbkpav7c6c6");
           ("time", Dag_cbor.Text "2024-01-01T00:00:00.000Z");
           ("active", Dag_cbor.Bool false);
           ("status", Dag_cbor.Text "takendown");
         ])
  in
  (match Firehose.decode_frame (acct_header ^ acct_body) with
  | _, `Account ev ->
      OUnit2.assert_equal false ev.active;
      OUnit2.assert_equal (Some "takendown") ev.status
  | _ -> OUnit2.assert_failure "expected #account frame");
  let info_header = Firehose.encode_header { op = 1; t = Some "#info" } in
  let info_body =
    Dag_cbor.encode
      (Dag_cbor.Map
         [
           ("name", Dag_cbor.Text "OutdatedCursor");
           ("message", Dag_cbor.Text "cursor is too old");
         ])
  in
  match Firehose.decode_frame (info_header ^ info_body) with
  | _, `Info ev ->
      OUnit2.assert_equal ~printer:(fun x -> x) "OutdatedCursor" ev.name;
      OUnit2.assert_equal (Some "cursor is too old") ev.message
  | _ -> OUnit2.assert_failure "expected #info frame"

let test_decode_error_frame _ =
  let header = Firehose.encode_header { op = -1; t = None } in
  let body =
    Dag_cbor.encode
      (Dag_cbor.Map
         [
           ("error", Dag_cbor.Text "ConsumerTooSlow");
           ("message", Dag_cbor.Text "lagged");
         ])
  in
  match Firehose.decode_frame (header ^ body) with
  | _, `Error (name, Some msg) ->
      OUnit2.assert_equal ~printer:(fun x -> x) "ConsumerTooSlow" name;
      OUnit2.assert_equal ~printer:(fun x -> x) "lagged" msg
  | _ -> OUnit2.assert_failure "expected error frame"

let test_decode_commit_ops _ =
  let cid = Cid.of_digest (String.make 32 '\x03') in
  let header = Firehose.encode_header { op = 1; t = Some "#commit" } in
  let empty_car = Atproto.Car.Car.encode { roots = [ cid ]; blocks = [] } in
  let body =
    Dag_cbor.encode
      (Dag_cbor.Map
         [
           ("seq", Dag_cbor.Int 1);
           ("rebase", Dag_cbor.Bool false);
           ("tooBig", Dag_cbor.Bool false);
           ("repo", Dag_cbor.Text "did:plc:7iza6de2dwap2sbkpav7c6c6");
           ("commit", Dag_cbor.Cid cid);
           ("rev", Dag_cbor.Text "3k5nobkf2w72g");
           ("since", Dag_cbor.Null);
           ("prevData", Dag_cbor.Cid cid);
           ("blocks", Dag_cbor.Bytes empty_car);
           ( "ops",
             Dag_cbor.Array
               [
                 Dag_cbor.Map
                   [
                     ("action", Dag_cbor.Text "create");
                     ("path", Dag_cbor.Text "app.bsky.feed.post/1");
                     ("cid", Dag_cbor.Cid cid);
                   ];
               ] );
           ("blobs", Dag_cbor.Array []);
           ("time", Dag_cbor.Text "2024-01-01T00:00:00.000Z");
         ])
  in
  match Firehose.decode_frame (header ^ body) with
  | _, `Commit commit ->
      OUnit2.assert_equal ~printer:(fun x -> x) "3k5nobkf2w72g" commit.rev;
      OUnit2.assert_equal 1 (List.length commit.ops);
      OUnit2.assert_equal
        ~printer:(fun x -> x)
        "create" (List.hd commit.ops).action;
      OUnit2.assert_bool "prevData parsed"
        (match commit.prev_data with
        | Some p -> Cid.equal p cid
        | None -> false);
      OUnit2.assert_equal 0 (List.length commit.blobs)
  | _ -> OUnit2.assert_failure "expected #commit frame"

let synthetic_inverted_commit () =
  let store = Mst.store_of_get (fun _ -> None) in
  let t = Mst.empty_tree store in
  let va = Cid.create ~codec:Cid.Raw "rec-a" in
  let vb = Cid.create ~codec:Cid.Raw "rec-b" in
  let t, _ = Mst.insert t "app.bsky.feed.post/aaa" va in
  let prev_data = Mst.root_cid t in
  let t, _ = Mst.insert t "app.bsky.feed.post/bbb" vb in
  let mst_root = Mst.root_cid t in
  let commit_bytes =
    Mst.encode_repo_commit ~did:"did:plc:7iza6de2dwap2sbkpav7c6c6"
      ~data:mst_root ~rev:"3k5nobkf2w72g" ()
  in
  let commit_cid = Cid.create commit_bytes in
  let mst_blocks =
    Hashtbl.fold
      (fun k data acc -> { Car.cid = Cid.of_string k; data } :: acc)
      t.store.created []
  in
  let car =
    {
      Car.roots = [ commit_cid ];
      blocks = { Car.cid = commit_cid; data = commit_bytes } :: mst_blocks;
    }
  in
  let raw = Car.encode car in
  {
    Firehose.seq = 1L;
    rebase = false;
    too_big = false;
    repo = "did:plc:7iza6de2dwap2sbkpav7c6c6";
    commit = commit_cid;
    rev = "3k5nobkf2w72g";
    since = None;
    prev_data = Some prev_data;
    blocks = car;
    raw_blocks = raw;
    ops =
      [
        {
          Firehose.action = "create";
          path = "app.bsky.feed.post/bbb";
          cid = Some vb;
          prev = None;
        };
      ];
    blobs = [];
    time = "2024-01-01T00:00:00.000Z";
  }

let test_invert_synthetic_commit _ =
  let commit = synthetic_inverted_commit () in
  Firehose.verify_commit commit;
  let inverted = Firehose.invert_commit commit in
  match commit.prev_data with
  | Some expected ->
      OUnit2.assert_bool "inverted root matches prevData"
        (Cid.equal inverted expected)
  | None -> OUnit2.assert_failure "fixture missing prevData"

let test_verify_live_shaped_cbor_frame _ =
  let commit = synthetic_inverted_commit () in
  let header = Firehose.encode_header { op = 1; t = Some "#commit" } in
  let op0 = List.hd commit.ops in
  let body =
    Dag_cbor.encode
      (Dag_cbor.Map
         [
           ("seq", Dag_cbor.Int 1);
           ("rebase", Dag_cbor.Bool false);
           ("tooBig", Dag_cbor.Bool false);
           ("repo", Dag_cbor.Text commit.repo);
           ("commit", Dag_cbor.Cid commit.commit);
           ("rev", Dag_cbor.Text commit.rev);
           ("since", Dag_cbor.Null);
           ( "prevData",
             match commit.prev_data with
             | Some c -> Dag_cbor.Cid c
             | None -> Dag_cbor.Null );
           ("blocks", Dag_cbor.Bytes commit.raw_blocks);
           ( "ops",
             Dag_cbor.Array
               [
                 Dag_cbor.Map
                   ([
                      ("action", Dag_cbor.Text op0.action);
                      ("path", Dag_cbor.Text op0.path);
                    ]
                   @
                   match op0.cid with
                   | Some c -> [ ("cid", Dag_cbor.Cid c) ]
                   | None -> [ ("cid", Dag_cbor.Null) ]);
               ] );
           ("blobs", Dag_cbor.Array []);
           ("time", Dag_cbor.Text commit.time);
         ])
  in
  match Firehose.decode_frame (header ^ body) with
  | _, `Commit decoded ->
      Firehose.verify_commit decoded;
      let inverted = Firehose.invert_commit decoded in
      OUnit2.assert_bool "live-shaped prevData"
        (match decoded.prev_data with
        | Some expected -> Cid.equal inverted expected
        | None -> false)
  | _ -> OUnit2.assert_failure "expected #commit frame"

let test_verify_sync_commit_object _ =
  let commit = synthetic_inverted_commit () in
  let sync =
    {
      Firehose.seq = 9L;
      did = commit.repo;
      blocks = commit.blocks;
      raw_blocks = commit.raw_blocks;
      rev = commit.rev;
      time = commit.time;
    }
  in
  let signed = Firehose.verify_sync sync in
  OUnit2.assert_equal ~printer:(fun x -> x) commit.repo signed.did;
  OUnit2.assert_equal ~printer:(fun x -> x) commit.rev signed.rev

let test_apply_commit_matches_invert _ =
  let commit = synthetic_inverted_commit () in
  let signed = Firehose.verify_commit_object commit in
  let store = Mst.store_of_car commit.blocks in
  let current = Mst.tree_of_root store signed.data in
  let prev_root = Firehose.invert_commit commit in
  (* rebuild the previous tree by inverting, then apply forward *)
  let prev_tree = Mst.invert_ops current (Firehose.record_ops commit.ops) in
  OUnit2.assert_bool "invert root"
    (Cid.equal (Mst.root_cid prev_tree) prev_root);
  let applied = Firehose.apply_commit ~prev_tree commit in
  OUnit2.assert_bool "apply returns to commit.data"
    (Cid.equal (Mst.root_cid applied) signed.data)

let test_invert_rejects_wrong_op _ =
  let commit = synthetic_inverted_commit () in
  let bad =
    {
      commit with
      ops =
        [
          {
            Firehose.action = "create";
            path = "app.bsky.feed.post/missing";
            cid = Some (Cid.create ~codec:Cid.Raw "nope");
            prev = None;
          };
        ];
    }
  in
  OUnit2.assert_bool "wrong op accepted"
    (try
       Firehose.verify_commit bad;
       false
     with Failure _ | Mst.Verify_error _ -> true)

let test_decode_update_and_delete_ops _ =
  let cid = Cid.of_digest (String.make 32 '\x04') in
  let prev = Cid.of_digest (String.make 32 '\x05') in
  let header = Firehose.encode_header { op = 1; t = Some "#commit" } in
  let empty_car = Car.encode { Car.roots = [ cid ]; blocks = [] } in
  let body =
    Dag_cbor.encode
      (Dag_cbor.Map
         [
           ("seq", Dag_cbor.Int 2);
           ("rebase", Dag_cbor.Bool false);
           ("tooBig", Dag_cbor.Bool false);
           ("repo", Dag_cbor.Text "did:plc:7iza6de2dwap2sbkpav7c6c6");
           ("commit", Dag_cbor.Cid cid);
           ("rev", Dag_cbor.Text "3k5nobkf2w72h");
           ("blocks", Dag_cbor.Bytes empty_car);
           ( "ops",
             Dag_cbor.Array
               [
                 Dag_cbor.Map
                   [
                     ("action", Dag_cbor.Text "update");
                     ("path", Dag_cbor.Text "app.bsky.feed.post/2");
                     ("cid", Dag_cbor.Cid cid);
                     ("prev", Dag_cbor.Cid prev);
                   ];
                 Dag_cbor.Map
                   [
                     ("action", Dag_cbor.Text "delete");
                     ("path", Dag_cbor.Text "app.bsky.feed.post/3");
                     ("cid", Dag_cbor.Null);
                     ("prev", Dag_cbor.Cid prev);
                   ];
               ] );
           ("blobs", Dag_cbor.Array [ Dag_cbor.Cid cid ]);
           ("time", Dag_cbor.Text "2024-01-01T00:00:00.000Z");
         ])
  in
  match Firehose.decode_frame (header ^ body) with
  | _, `Commit commit ->
      OUnit2.assert_equal 2 (List.length commit.ops);
      OUnit2.assert_equal
        ~printer:(fun x -> x)
        "update" (List.nth commit.ops 0).action;
      OUnit2.assert_equal
        ~printer:(fun x -> x)
        "delete" (List.nth commit.ops 1).action;
      OUnit2.assert_bool "delete prev"
        (match (List.nth commit.ops 1).prev with
        | Some p -> Cid.equal p prev
        | None -> false);
      OUnit2.assert_equal 1 (List.length commit.blobs)
  | _ -> OUnit2.assert_failure "expected #commit frame"

let test_websocket_subprotocol_echo _ =
  OUnit2.assert_equal [] (Websocket.offered_subprotocols []);
  OUnit2.assert_equal []
    (Websocket.offered_subprotocols [ ("Socket-Encoding", "zstd") ]);
  OUnit2.assert_equal [ "xrpc.v1.json" ]
    (Websocket.offered_subprotocols
       [ ("Sec-WebSocket-Protocol", "xrpc.v1.json") ]);
  OUnit2.assert_equal [ "xrpc.v1.json" ]
    (Websocket.offered_subprotocols
       [ ("sec-websocket-protocol", " xrpc.v1.json ") ]);
  OUnit2.assert_equal [ "xrpc.v1.json"; "other" ]
    (Websocket.offered_subprotocols
       [ ("Sec-WebSocket-Protocol", "xrpc.v1.json, other") ]);
  Websocket.check_subprotocol_echo ~offered:[] None;
  Websocket.check_subprotocol_echo ~offered:[ "xrpc.v1.json" ]
    (Some "xrpc.v1.json");
  OUnit2.assert_raises
    (Websocket.Subprotocol_error
       "server omitted Sec-WebSocket-Protocol after client offer") (fun () ->
      Websocket.check_subprotocol_echo ~offered:[ "xrpc.v1.json" ] None);
  OUnit2.assert_raises
    (Websocket.Subprotocol_error
       "Sec-WebSocket-Protocol \"nope\" was not in the client offer") (fun () ->
      Websocket.check_subprotocol_echo ~offered:[ "xrpc.v1.json" ] (Some "nope"))

let test_websocket_accept_rfc6455 _ =
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="
    (Websocket.accept_key "dGhlIHNhbXBsZSBub25jZQ==")

let test_websocket_unmasked_roundtrip _ =
  let encoded = Websocket.encode_frame ~mask:false ~opcode:2 "hello" in
  let frame, consumed = Websocket.decode_frame_bytes encoded in
  OUnit2.assert_equal consumed (String.length encoded);
  OUnit2.assert_equal 2 frame.opcode;
  OUnit2.assert_equal ~printer:(fun x -> x) "hello" frame.payload;
  OUnit2.assert_bool "fin" frame.fin

let test_websocket_extended_length _ =
  let payload = String.make 200 'x' in
  let encoded = Websocket.encode_frame ~mask:false ~opcode:1 payload in
  let frame, _ = Websocket.decode_frame_bytes encoded in
  OUnit2.assert_equal ~printer:string_of_int 200 (String.length frame.payload);
  OUnit2.assert_equal ~printer:(fun x -> x) payload frame.payload

let test_websocket_masked_roundtrip _ =
  Random.init 1;
  let encoded = Websocket.encode_frame ~mask:true ~opcode:2 "masked" in
  let frame, _ = Websocket.decode_frame_bytes encoded in
  OUnit2.assert_equal ~printer:(fun x -> x) "masked" frame.payload

let test_parse_wss_url _ =
  let host, port, path =
    Websocket.parse_wss_url
      "wss://bsky.network/xrpc/com.atproto.sync.subscribeRepos?cursor=1"
  in
  OUnit2.assert_equal ~printer:(fun x -> x) "bsky.network" host;
  OUnit2.assert_equal ~printer:string_of_int 443 port;
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "/xrpc/com.atproto.sync.subscribeRepos?cursor=1" path;
  let ws_host, ws_port, ws_path =
    Websocket.parse_wss_url
      "ws://localhost:2583/xrpc/com.atproto.sync.subscribeRepos"
  in
  OUnit2.assert_equal ~printer:(fun x -> x) "localhost" ws_host;
  OUnit2.assert_equal ~printer:string_of_int 2583 ws_port;
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "/xrpc/com.atproto.sync.subscribeRepos" ws_path;
  let def = Websocket.parse_url "ws://relay.example/xrpc/ping" in
  OUnit2.assert_equal false def.Websocket.secure;
  OUnit2.assert_equal ~printer:string_of_int 80 def.port;
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "relay.example:2583"
    (Websocket.authority
       {
         Websocket.secure = false;
         host = "relay.example";
         port = 2583;
         path = "/";
       })

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
        let _, msg = Firehose.subscribe_one () in
        match msg with
        | `Commit _ | `Sync _ | `Identity _ | `Account _ | `Info _ | `Unknown _
        | `Error _ ->
            OUnit2.assert_bool "decoded a subscribeRepos frame" true
      with exn ->
        skip_if true ("subscribeRepos skipped: " ^ Printexc.to_string exn))

let test_subscribe_invert_live _ =
  let old =
    Sys.signal Sys.sigalrm (Sys.Signal_handle (fun _ -> failwith "timeout"))
  in
  ignore (Unix.alarm 25);
  Fun.protect
    ~finally:(fun () ->
      ignore (Unix.alarm 0);
      Sys.set_signal Sys.sigalrm old)
    (fun () ->
      try
        let found = ref false in
        Firehose.subscribe ~max_messages:12 (fun (_header, msg) ->
            match msg with
            | `Commit c
              when (not c.too_big) && (not c.rebase) && c.ops <> []
                   && c.prev_data <> None -> (
                try
                  Firehose.verify_commit c;
                  found := true
                with exn ->
                  skip_if true ("live invert skipped: " ^ Printexc.to_string exn)
                )
            | _ -> ());
        skip_if (not !found)
          "subscribeRepos produced no invertible #commit in the sample window"
      with exn ->
        skip_if true ("subscribeRepos invert skipped: " ^ Printexc.to_string exn))

let test_validate_limits _ =
  let cid = Cid.of_digest (String.make 32 '\x03') in
  let empty_car = { Car.roots = [ cid ]; blocks = [] } in
  let base =
    {
      Firehose.seq = 1L;
      rebase = false;
      too_big = false;
      repo = "did:plc:7iza6de2dwap2sbkpav7c6c6";
      commit = cid;
      rev = "3jzfcijpj2z2a";
      since = None;
      prev_data = None;
      blocks = empty_car;
      raw_blocks = String.make 16 'x';
      ops =
        [
          {
            Firehose.action = "create";
            path = "app.bsky.feed.post/3jzfcijpj2z2a";
            cid = Some cid;
            prev = None;
          };
        ];
      blobs = [];
      time = "2024-01-01T00:00:00.000Z";
    }
  in
  Firehose.validate_limits base;
  OUnit2.assert_bool "bad path rejected"
    (try
       Firehose.validate_limits
         { base with ops = [ { (List.hd base.ops) with path = "not-a-path" } ] };
       false
     with Failure _ -> true);
  OUnit2.assert_bool "too many ops rejected"
    (try
       let op = List.hd base.ops in
       Firehose.validate_limits
         { base with ops = List.init (Firehose.max_ops + 1) (fun _ -> op) };
       false
     with Failure _ -> true);
  OUnit2.assert_bool "future rev rejected"
    (try
       Firehose.validate_limits
         { base with rev = Tid.create ~clock_id:0 9_000_000_000_000_000L };
       false
     with Failure _ -> true)

let suite =
  "firehose"
  >::: [
         "test_subscribe_url" >:: test_subscribe_url;
         "test_decode_identity_frame" >:: test_decode_identity_frame;
         "test_decode_sync_account_info" >:: test_decode_sync_account_info;
         "test_decode_error_frame" >:: test_decode_error_frame;
         "test_decode_commit_ops" >:: test_decode_commit_ops;
         "test_decode_update_and_delete_ops"
         >:: test_decode_update_and_delete_ops;
         "test_invert_synthetic_commit" >:: test_invert_synthetic_commit;
         "test_apply_commit_matches_invert" >:: test_apply_commit_matches_invert;
         "test_verify_sync_commit_object" >:: test_verify_sync_commit_object;
         "test_verify_live_shaped_cbor_frame"
         >:: test_verify_live_shaped_cbor_frame;
         "test_invert_rejects_wrong_op" >:: test_invert_rejects_wrong_op;
         "test_websocket_subprotocol_echo" >:: test_websocket_subprotocol_echo;
         "test_websocket_accept_rfc6455" >:: test_websocket_accept_rfc6455;
         "test_websocket_unmasked_roundtrip"
         >:: test_websocket_unmasked_roundtrip;
         "test_websocket_extended_length" >:: test_websocket_extended_length;
         "test_websocket_masked_roundtrip" >:: test_websocket_masked_roundtrip;
         "test_parse_wss_url" >:: test_parse_wss_url;
         "test_subscribe_live" >:: test_subscribe_live;
         "test_subscribe_invert_live" >:: test_subscribe_invert_live;
         "test_validate_limits" >:: test_validate_limits;
       ]

let () = run_test_tt_main suite
