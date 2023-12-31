open OUnit2
open Atproto.Session
open Atproto.Auth
open Atproto.Sync

let create_test_session _ =
    let (username, password) = Auth.username_and_password_from_env in
    Session.create_session username password

let test_get_blob _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let blob = Lwt_main.run (Sync.get_blob test_session "did:plc:xov3uvxfd4to6ev3ak5g5uxk" "bafkreieva64qpnxs7zmwc6ezo7hatq4d22ot7wqlj4hi24zimjqzoye4wq") in
  OUnit2.assert_bool "Blob is not empty" (blob <> "")


let suite =
    "suite"
    >::: [
         "test_get_blob" >:: test_get_blob;
         ]

let () = run_test_tt_main suite
