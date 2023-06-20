open OUnit2
open Atproto.Session
open Atproto.Auth
open Atproto.Feed

let test_get_author_feed _ =
  let (username, password) = Auth.username_and_password_from_env in
  let test_session = Session.create_session username password in
  let author_feed = Feed.get_author_feed test_session "david-engelmann.bsky.social" 10 in
  Printf.printf "Author Feed: %s\n" author_feed;
  OUnit2.assert_bool "Author Feed is not empty" (author_feed <> "")


let suite =
    "suite"
    >::: [
           "test_get_author_feed" >:: test_get_author_feed;
         ]

let () = run_test_tt_main suite
