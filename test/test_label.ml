open OUnit2
open Atproto.Session
open Atproto.Auth
open Atproto.Label

let create_test_session _ =
  let username, password = Auth.username_and_password_from_env in
  Session.create_session username password

let test_query_labels _ =
  skip_if
    (not Auth.has_live_credentials)
    "ATP_AUTH not configured; live Bluesky test skipped";
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let labels = Label.query_labels test_session [ "*" ] in
  Printf.printf "Query Labels: %s\n" labels;
  OUnit2.assert_bool "Query Labels is not empty" (labels <> "")

let test_parse_query_labels _ =
  let json =
    `Assoc
      [
        ("cursor", `String "c1");
        ( "labels",
          `List
            [
              `Assoc
                [
                  ("src", `String "did:plc:labeler");
                  ("uri", `String "at://did:plc:alice/app.bsky.feed.post/1");
                  ("val", `String "!warn");
                  ("neg", `Bool false);
                  ("cts", `String "2024-01-01T00:00:00.000Z");
                  ("ver", `Int 1);
                ];
            ] );
      ]
  in
  let q = Label.parse_query_labels json in
  OUnit2.assert_equal (Some "c1") q.cursor;
  OUnit2.assert_equal 1 (List.length q.labels);
  let label = List.hd q.labels in
  OUnit2.assert_equal ~printer:(fun x -> x) "!warn" label.val_;
  OUnit2.assert_equal ~printer:(fun x -> x) "did:plc:labeler" label.src;
  OUnit2.assert_equal (Some 1) label.ver

let sample_label : Label.label =
  {
    src = "did:plc:ewvi7nxzyoun6zhxrhs64oiz";
    uri = "at://did:plc:alice/app.bsky.feed.post/1";
    cid = None;
    val_ = "!warn";
    neg = None;
    cts = Some "2024-01-01T00:00:00.000Z";
    exp = None;
    ver = Some 1;
    sig_ = None;
  }

let test_label_sign_verify_p256 _ =
  let priv_hex =
    "c9afa9d845ba75166b5c215767b1d6934e50c3db36e89b127b8a622b120f6721"
  in
  let priv =
    match
      Mirage_crypto_ec.P256.Dsa.priv_of_octets
        (Atproto.Hash.Hash.hex_decode priv_hex)
    with
    | Ok p -> p
    | Error _ -> failwith "p256 key"
  in
  let pub = Mirage_crypto_ec.P256.Dsa.pub_of_priv priv in
  let octets = Mirage_crypto_ec.P256.Dsa.pub_to_octets ~compress:true pub in
  let did_key = Atproto.Did_key.Did_key.(to_string (of_p256_octets octets)) in
  let signed = Label.sign_p256 ~priv sample_label in
  OUnit2.assert_bool "sig missing"
    (match signed.sig_ with Some s -> String.length s = 64 | None -> false);
  OUnit2.assert_equal `Valid (Label.verify_with_keys ~keys:[ did_key ] signed);
  let tampered = { signed with val_ = "scam" } in
  OUnit2.assert_equal `Invalid
    (Label.verify_with_keys ~keys:[ did_key ] tampered);
  OUnit2.assert_equal `Missing
    (Label.verify_with_keys ~keys:[ did_key ] sample_label)

let test_label_sign_verify_k256 _ =
  let d = String.make 32 '\x01' in
  let priv =
    match Atproto.K256.K256.priv_of_octets d with
    | Ok p -> p
    | Error _ -> failwith "k256 key"
  in
  let pub = Atproto.K256.K256.pub_of_priv priv in
  let octets = Atproto.K256.K256.pub_to_octets ~compress:true pub in
  let did_key = Atproto.Did_key.Did_key.(to_string (of_k256_octets octets)) in
  let signed = Label.sign_k256 ~priv sample_label in
  OUnit2.assert_equal `Valid (Label.verify_with_keys ~keys:[ did_key ] signed)

let test_subscribe_labels_frame _ =
  let frame =
    Label.encode_labels_frame { seq = 7L; labels = [ sample_label ] }
  in
  let header, msg = Label.decode_frame frame in
  OUnit2.assert_equal 1 header.op;
  OUnit2.assert_equal (Some "#labels") header.t;
  match msg with
  | `Labels m ->
      OUnit2.assert_equal ~printer:Int64.to_string 7L m.seq;
      OUnit2.assert_equal 1 (List.length m.labels);
      OUnit2.assert_equal ~printer:(fun x -> x) "!warn" (List.hd m.labels).val_
  | _ -> OUnit2.assert_failure "expected #labels"

let test_subscribe_url _ =
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "wss://mod.example.com/xrpc/com.atproto.label.subscribeLabels?cursor=0"
    (Label.subscribe_url ~host:"mod.example.com" ~cursor:0L ())

let test_json_sig_roundtrip _ =
  let json =
    `Assoc
      [
        ("src", `String "did:plc:labeler");
        ("uri", `String "did:plc:alice");
        ("val", `String "spam");
        ("ver", `Int 1);
        ("cts", `String "2024-01-01T00:00:00.000Z");
        ( "sig",
          `Assoc
            [ ("$bytes", `String (Atproto.Base64url.Base64url.encode_std "abcd")) ]
        );
      ]
  in
  let l = Label.parse_label json in
  OUnit2.assert_equal (Some "abcd") l.sig_;
  let back = Label.json_of_label l in
  let again = Label.parse_label back in
  OUnit2.assert_equal again.sig_ l.sig_

let suite =
  "suite"
  >::: [
         "test_query_labels" >:: test_query_labels;
         "test_parse_query_labels" >:: test_parse_query_labels;
         "test_label_sign_verify_p256" >:: test_label_sign_verify_p256;
         "test_label_sign_verify_k256" >:: test_label_sign_verify_k256;
         "test_subscribe_labels_frame" >:: test_subscribe_labels_frame;
         "test_subscribe_url" >:: test_subscribe_url;
         "test_json_sig_roundtrip" >:: test_json_sig_roundtrip;
       ]

let () = run_test_tt_main suite
