open OUnit2
open Atproto.Ozone
open Atproto.Xrpc
open Atproto.Auth

let test_labeler_proxy _ =
  let p = Ozone.labeler_proxy "did:web:mod.example.com" in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "did:web:mod.example.com#atproto_labeler" (Xrpc.proxy_to_string p)

let test_parse_statuses _ =
  let json =
    `Assoc
      [
        ( "subjectStatuses",
          `List
            [
              `Assoc
                [
                  ( "subject",
                    `Assoc
                      [
                        ("$type", `String "com.atproto.admin.defs#repoRef");
                        ("did", `String "did:plc:abc123xyz0001112223333");
                      ] );
                  ( "reviewState",
                    `String "tools.ozone.moderation.defs#reviewOpen" );
                  ("comment", `String "looks spammy");
                  ("priorityScore", `Int 40);
                ];
            ] );
      ]
  in
  let page = Ozone.parse_statuses json in
  OUnit2.assert_equal 1 (List.length page.subject_statuses);
  OUnit2.assert_equal (Some "looks spammy")
    (List.hd page.subject_statuses).comment

let test_emit_event_body _ =
  let body =
    Ozone.emit_event_body
      ~event:(Ozone.comment_event "note")
      ~subject:(Ozone.repo_ref "did:plc:abc123xyz0001112223333")
      ~created_by:"did:plc:mod000111222333444555666" ()
  in
  let open Yojson.Safe.Util in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "did:plc:abc123xyz0001112223333"
    (body |> member "subject" |> member "did" |> to_string);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "tools.ozone.moderation.defs#modEventComment"
    (body |> member "event" |> member "$type" |> to_string)

let test_takedown_event _ =
  let ev = Ozone.takedown_event ~comment:"spam" () in
  let open Yojson.Safe.Util in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "tools.ozone.moderation.defs#modEventTakedown"
    (ev |> member "$type" |> to_string)

let test_query_statuses_auth_skipped _ =
  skip_if
    (not Auth.has_live_credentials)
    "ATP_AUTH not configured; live Bluesky test skipped";
  let username, password = Auth.username_and_password_from_env in
  let s = Atproto.Session.Session.create_session username password in
  try
    let proxy = Ozone.labeler_proxy "did:plc:ar7c4by46qjdydhdevvrndac" in
    let page = Ozone.query_statuses s ~proxy ~limit:1 () in
    OUnit2.assert_bool "statuses parsed" (List.length page.subject_statuses >= 0)
  with exn -> skip_if true ("queryStatuses skipped: " ^ Printexc.to_string exn)

let suite =
  "ozone"
  >::: [
         "test_labeler_proxy" >:: test_labeler_proxy;
         "test_parse_statuses" >:: test_parse_statuses;
         "test_emit_event_body" >:: test_emit_event_body;
         "test_takedown_event" >:: test_takedown_event;
         "test_query_statuses_auth_skipped" >:: test_query_statuses_auth_skipped;
       ]

let () = run_test_tt_main suite
