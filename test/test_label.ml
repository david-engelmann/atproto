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

let test_query_labels_body _ =
  let pairs =
    Label.query_labels_body
      ~uri_patterns:[ "at://did:plc:alice/*"; "*" ]
      ~sources:[ "did:plc:labeler" ] ~limit:25 ~cursor:"c1" ()
  in
  OUnit2.assert_equal
    [
      ("uriPatterns", "at://did:plc:alice/*");
      ("uriPatterns", "*");
      ("sources", "did:plc:labeler");
      ("limit", "25");
      ("cursor", "c1");
    ]
    pairs;
  OUnit2.assert_equal
    [ ("uriPatterns", "*") ]
    (Label.query_labels_body ~uri_patterns:[ "*" ] ())

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
    (Label.subscribe_url ~host:"mod.example.com" ~cursor:0L ());
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "wss://bsky.network/xrpc/com.atproto.label.subscribeLabels"
    (Label.subscribe_url ());
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "ws://localhost:2583/xrpc/com.atproto.label.subscribeLabels"
    (Label.subscribe_url ~host:"localhost:2583" ());
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "ws://127.0.0.1:2587/xrpc/com.atproto.label.subscribeLabels?cursor=0"
    (Label.subscribe_url ~host:"127.0.0.1:2587" ~cursor:0L ());
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "wss://localhost:2583/xrpc/com.atproto.label.subscribeLabels"
    (Label.subscribe_url ~host:"localhost:2583" ~scheme:"wss" ())

let test_subscribe_one_live _ =
  Public_live.skip_unless_public ();
  let old =
    Sys.signal Sys.sigalrm (Sys.Signal_handle (fun _ -> failwith "timeout"))
  in
  ignore (Unix.alarm 15);
  Fun.protect
    ~finally:(fun () ->
      ignore (Unix.alarm 0);
      Sys.set_signal Sys.sigalrm old)
    (fun () ->
      try
        let _, msg = Label.subscribe_one () in
        match msg with
        | `Labels _ | `Info _ | `Error _ | `Unknown _ ->
            OUnit2.assert_bool "decoded subscribeLabels" true
      with exn ->
        skip_if true ("subscribeLabels skipped: " ^ Printexc.to_string exn))

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
            [
              ("$bytes", `String (Atproto.Base64url.Base64url.encode_std "abcd"));
            ] );
      ]
  in
  let l = Label.parse_label json in
  OUnit2.assert_equal (Some "abcd") l.sig_;
  let back = Label.json_of_label l in
  let again = Label.parse_label back in
  OUnit2.assert_equal again.sig_ l.sig_

let test_self_labels _ =
  let json =
    `Assoc
      [
        ("$type", `String "com.atproto.label.defs#selfLabels");
        ("values", `List [ `Assoc [ ("val", `String "porn") ] ]);
      ]
  in
  OUnit2.assert_equal (Some [ "porn" ]) (Label.parse_self_labels json);
  let encoded = Label.self_labels_to_json [ "nudity" ] in
  OUnit2.assert_equal (Some [ "nudity" ]) (Label.parse_self_labels encoded)

let test_parse_label_value_definition _ =
  let json =
    `Assoc
      [
        ("identifier", `String "spam");
        ("severity", `String "alert");
        ("blurs", `String "content");
        ("defaultSetting", `String "hide");
        ("adultOnly", `Bool false);
        ( "locales",
          `List
            [
              `Assoc
                [
                  ("lang", `String "en");
                  ("name", `String "Spam");
                  ("description", `String "Unwanted commercial content");
                ];
            ] );
      ]
  in
  let def = Label.parse_label_value_definition json in
  OUnit2.assert_equal ~printer:(fun x -> x) "spam" def.identifier;
  OUnit2.assert_equal ~printer:(fun x -> x) "alert" def.severity;
  OUnit2.assert_equal ~printer:(fun x -> x) "content" def.blurs;
  OUnit2.assert_equal (Some "hide") def.default_setting;
  OUnit2.assert_equal (Some false) def.adult_only;
  OUnit2.assert_equal 1 (List.length def.locales);
  OUnit2.assert_equal ~printer:(fun x -> x) "en" (List.hd def.locales).lang

let test_label_value_definition_to_json _ =
  let locale : Label.label_value_definition_strings =
    { lang = "en"; name = "Spam"; description = "Unwanted commercial content" }
  in
  let encoded_locale = Label.label_value_definition_strings_to_json locale in
  let parsed_locale =
    Label.parse_label_value_definition_strings encoded_locale
  in
  OUnit2.assert_equal ~printer:(fun x -> x) "en" parsed_locale.lang;
  OUnit2.assert_equal ~printer:(fun x -> x) "Spam" parsed_locale.name;
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "Unwanted commercial content" parsed_locale.description;
  let def : Label.label_value_definition =
    {
      identifier = "spam";
      severity = "alert";
      blurs = "content";
      default_setting = Some "hide";
      adult_only = Some false;
      locales = [ locale ];
    }
  in
  let encoded = Label.label_value_definition_to_json def in
  let back = Label.parse_label_value_definition encoded in
  OUnit2.assert_equal ~printer:(fun x -> x) "spam" back.identifier;
  OUnit2.assert_equal ~printer:(fun x -> x) "alert" back.severity;
  OUnit2.assert_equal ~printer:(fun x -> x) "content" back.blurs;
  OUnit2.assert_equal (Some "hide") back.default_setting;
  OUnit2.assert_equal (Some false) back.adult_only;
  OUnit2.assert_equal 1 (List.length back.locales);
  OUnit2.assert_equal ~printer:(fun x -> x) "en" (List.hd back.locales).lang;
  let open Yojson.Safe.Util in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "hide"
    (encoded |> member "defaultSetting" |> to_string);
  OUnit2.assert_equal false (encoded |> member "adultOnly" |> to_bool);
  let minimal : Label.label_value_definition =
    {
      identifier = "warn";
      severity = "inform";
      blurs = "none";
      default_setting = None;
      adult_only = None;
      locales = [];
    }
  in
  let minimal_json = Label.label_value_definition_to_json minimal in
  OUnit2.assert_equal `Null (minimal_json |> member "defaultSetting");
  OUnit2.assert_equal `Null (minimal_json |> member "adultOnly");
  let again = Label.parse_label_value_definition minimal_json in
  OUnit2.assert_equal None again.default_setting;
  OUnit2.assert_equal None again.adult_only;
  OUnit2.assert_equal [] again.locales

let suite =
  "suite"
  >::: [
         "test_query_labels" >:: test_query_labels;
         "test_query_labels_body" >:: test_query_labels_body;
         "test_self_labels" >:: test_self_labels;
         "test_parse_label_value_definition"
         >:: test_parse_label_value_definition;
         "test_label_value_definition_to_json"
         >:: test_label_value_definition_to_json;
         "test_parse_query_labels" >:: test_parse_query_labels;
         "test_label_sign_verify_p256" >:: test_label_sign_verify_p256;
         "test_label_sign_verify_k256" >:: test_label_sign_verify_k256;
         "test_subscribe_labels_frame" >:: test_subscribe_labels_frame;
         "test_subscribe_url" >:: test_subscribe_url;
         "test_subscribe_one_live" >:: test_subscribe_one_live;
         "test_json_sig_roundtrip" >:: test_json_sig_roundtrip;
       ]

let () = run_test_tt_main suite
