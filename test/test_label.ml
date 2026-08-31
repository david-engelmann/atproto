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

let suite =
  "suite"
  >::: [
         "test_query_labels" >:: test_query_labels;
         "test_parse_query_labels" >:: test_parse_query_labels;
       ]
let () = run_test_tt_main suite
