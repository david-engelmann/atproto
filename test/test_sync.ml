open OUnit2
open Atproto.Session
open Atproto.Auth
open Atproto.Sync

let create_test_session _ =
    let (username, password) = Auth.username_and_password_from_env in
    Session.create_session username password

let test_get_blob _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let blob = Sync.get_blob test_session "did:plc:xov3uvxfd4to6ev3ak5g5uxk/app.bsky.feed.post/3jyf6gx25eb27" "bafyreiarimgpoqvxxnf3sg4h52gvfzvmyeybxk2xgy6v3dra7zuldy73aq" in
  Printf.printf "Blob: %s\n" blob;
  OUnit2.assert_bool "Blob is not empty" (blob <> "")


let suite =
    "suite"
    >::: [
          "test_get_blob" >:: test_get_blob;
         ]

let () = run_test_tt_main suite
