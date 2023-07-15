open OUnit2
open Atproto.Session
open Atproto.Auth
open Atproto.Sync

let create_test_session _ =
    let (username, password) = Auth.username_and_password_from_env in
    Session.create_session username password

let test_get_checkout _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let checkout = Sync.get_checkout test_session "did:plc:xov3uvxfd4to6ev3ak5g5uxk" "bafkreieva64qpnxs7zmwc6ezo7hatq4d22ot7wqlj4hi24zimjqzoye4wq" in
  Printf.printf "Checkout: %s\n" checkout;
  OUnit2.assert_bool "Checkout is not empty" (checkout <> "")

let test_get_commit_path _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let commit_path = Sync.get_commit_path test_session "did:plc:xov3uvxfd4to6ev3ak5g5uxk" "bafyreicdc7gergmcivdaw76rhinnevfecaxxnzhquukq6xbz5vh7fp2izi" "bafyreiarimgpoqvxxnf3sg4h52gvfzvmyeybxk2xgy6v3dra7zuldy73aq" in
  Printf.printf "Commit Path: %s\n" commit_path;
  OUnit2.assert_bool "Commit Path is not empty" (commit_path <> "")

  

let suite =
    "suite"
    >::: [
          "test_get_checkout" >:: test_get_checkout;
          "test_get_commit_path" >:: test_get_commit_path;

         ]

let () = run_test_tt_main suite
