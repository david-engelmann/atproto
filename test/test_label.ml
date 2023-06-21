open OUnit2
open Atproto.Session
open Atproto.Auth
open Atproto.Label

let create_test_session _ =
    let (username, password) = Auth.username_and_password_from_env in
    Session.create_session username password

let test_query_labels _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let labels = Label.query_labels test_session ["a"; "b"] in
  Printf.printf "Query Labels: %s\n" labels;
  OUnit2.assert_bool "Query Labels is not empty" (labels <> "")

let suite =
    "suite"
    >::: [
           "test_query_labels" >:: test_query_labels;
         ]

let () = run_test_tt_main suite
