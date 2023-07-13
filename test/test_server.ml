open OUnit2
open Atproto.Session
open Atproto.Auth
open Atproto.Server

let create_test_session _ =
    let (username, password) = Auth.username_and_password_from_env in
    Session.create_session username password

let test_describe_server _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let server_description = Server.describe_server test_session in
  Printf.printf "Server Description: %s\n" server_description;
  OUnit2.assert_bool "Blob is not empty" (server_description <> "")


let suite =
    "suite"
    >::: [
          "test_describe_server" >:: test_describe_server;
         ]

let () = run_test_tt_main suite
