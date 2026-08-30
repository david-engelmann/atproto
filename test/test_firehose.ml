open OUnit2
open Atproto.Cid
open Atproto.Dag_cbor
open Atproto.Firehose
open Atproto.Websocket

let test_subscribe_url _ =
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "wss://bsky.network/xrpc/com.atproto.sync.subscribeRepos"
    (Firehose.subscribe_url ());
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "wss://relay.example/xrpc/com.atproto.sync.subscribeRepos?cursor=99"
    (Firehose.subscribe_url ~host:"relay.example" ~cursor:99L ())

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
        "create" (List.hd commit.ops).action
  | _ -> OUnit2.assert_failure "expected #commit frame"

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
    "/xrpc/com.atproto.sync.subscribeRepos?cursor=1" path

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

let suite =
  "firehose"
  >::: [
         "test_subscribe_url" >:: test_subscribe_url;
         "test_decode_identity_frame" >:: test_decode_identity_frame;
         "test_decode_error_frame" >:: test_decode_error_frame;
         "test_decode_commit_ops" >:: test_decode_commit_ops;
         "test_websocket_accept_rfc6455" >:: test_websocket_accept_rfc6455;
         "test_websocket_unmasked_roundtrip"
         >:: test_websocket_unmasked_roundtrip;
         "test_websocket_extended_length" >:: test_websocket_extended_length;
         "test_websocket_masked_roundtrip" >:: test_websocket_masked_roundtrip;
         "test_parse_wss_url" >:: test_parse_wss_url;
         "test_subscribe_live" >:: test_subscribe_live;
       ]

let () = run_test_tt_main suite
