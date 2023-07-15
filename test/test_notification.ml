open OUnit2
open Atproto.Session
open Atproto.Auth
open Atproto.Notification

let create_test_session _ =
    let (username, password) = Auth.username_and_password_from_env in
    Session.create_session username password


let suite =
    "suite"
    >::: [


         ]

let () = run_test_tt_main suite
