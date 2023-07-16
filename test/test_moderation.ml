open OUnit2
open Atproto.Session
open Atproto.Auth
open Atproto.Moderation

let sample_strong_ref : Moderation.strong_ref = {
  uri = "at://did:plc:xov3uvxfd4to6ev3ak5g5uxk/app.bsky.feed.post/3jys3bxu3bt2m";
  cid = "bafyreihikeyzp2bd7k4zeywtcxbate7rhx4bkkcrzjlweisiejl5lypom4"
}

let create_test_session _ =
    let (username, password) = Auth.username_and_password_from_env in
    Session.create_session username password

let test_create_report_no_reason _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let created_report = Moderation.create_report_with_strong_ref test_session "com.atproto.moderation.defs#reasonOther" sample_strong_ref in
  Printf.printf "Moderation Report: %s\n" created_report;
  OUnit2.assert_bool "Moderation Report is not empty" (created_report <> "")

let suite =
    "suite"
    >::: [
          "test_create_report_no_reason" >:: test_create_report_no_reason;
         ]

let () = run_test_tt_main suite
