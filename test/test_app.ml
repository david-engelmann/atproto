open OUnit2
open Atproto.Session
open Atproto.Auth
open Atproto.App

let create_test_session _ =
    let (username, password) = Auth.username_and_password_from_env in
    Session.create_session username password

let test_create_base_url _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let test_base_url = App.create_base_url test_session in
  OUnit2.assert_equal "https://bsky.social/xrpc/" test_base_url

let test_create_endpoint_url _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let test_base_url = App.create_base_url test_session in
  let test_endpoint_url = App.create_endpoint_url test_base_url "test" in
  OUnit2.assert_equal "https://bsky.social/xrpc/test" test_endpoint_url


let suite =
  "suite"
  >::: [
         "test_create_base_url" >:: test_create_base_url;
         "test_create_endpoint_url" >:: test_create_endpoint_url;
       ]

let () = run_test_tt_main suite
