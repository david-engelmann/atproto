open OUnit2
open Atproto.Session
open Atproto.Auth
open Atproto.Repo

let create_test_session _ =
    let (username, password) = Auth.username_and_password_from_env in
    Session.create_session username password

let test_describe_repo _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let repo_description = Repo.describe_repo test_session "did:plc:xov3uvxfd4to6ev3ak5g5uxk" in
  Printf.printf "Repo Description: %s\n" repo_description;
  OUnit2.assert_bool "Repo Description is not empty" (repo_description <> "")

let suite =
    "suite"
    >::: [
           "test_describe_repo" >:: test_describe_repo;
         ]

let () = run_test_tt_main suite
