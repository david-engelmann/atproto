open OUnit2
open Atproto.Session
open Atproto.Auth
open Atproto.Moderation

let create_test_session _ =
    let (username, password) = Auth.username_and_password_from_env in
    Session.create_session username password

let test_create_report_no_reason _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let created_report = Moderation.create_report test_session "com.atproto.moderation.defs#reasonOther" "did:plc:yk4dd2qkboz2yv6tpubpc6co" in
  Printf.printf "Moderation Report: %s\n" created_report;
  OUnit2.assert_bool "Moderation Report is not empty" (created_report <> "")

let suite =
    "suite"
    >::: [
          "test_create_report_no_reason" >:: test_create_report_no_reason;
         ]

let () = run_test_tt_main suite
