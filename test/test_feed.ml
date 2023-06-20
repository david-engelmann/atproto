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

let test_get_likes _ =
  let (username, password) = Auth.username_and_password_from_env in
  let test_session = Session.create_session username password in
  let likes = Feed.get_likes test_session "at://did:plc:xov3uvxfd4to6ev3ak5g5uxk/app.bsky.feed.post/3jyf6gx25eb27" "bafyreiarimgpoqvxxnf3sg4h52gvfzvmyeybxk2xgy6v3dra7zuldy73aq" 1 in
  Printf.printf "Likes Feed: %s\n" likes;
  OUnit2.assert_bool "Likes Feed is not empty" (likes <> "")

(*  *)

let suite =
    "suite"
    >::: [
           "test_get_author_feed" >:: test_get_author_feed;
           "test_get_likes" >:: test_get_likes;
         ]

let () = run_test_tt_main suite
