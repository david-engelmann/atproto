open OUnit2
open Atproto.Session
open Atproto.Auth
open Atproto.Sync

let create_test_session _ =
    let (username, password) = Auth.username_and_password_from_env in
    Session.create_session username password

let test_get_blocks _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let blocks = Lwt_main.run (Sync.get_blocks test_session "did:plc:xov3uvxfd4to6ev3ak5g5uxk" ["bafkreieva64qpnxs7zmwc6ezo7hatq4d22ot7wqlj4hi24zimjqzoye4wq"]) in
  OUnit2.assert_bool "Blocks is not empty" (blocks <> "")


let suite =
    "suite"
    >::: [
         "test_get_blocks" >:: test_get_blocks;
         ]

let () = run_test_tt_main suite
