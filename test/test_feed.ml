open OUnit2
open Atproto.Session
open Atproto.Auth
open Atproto.Feed

let create_test_session _ =
    let (username, password) = Auth.username_and_password_from_env in
    Session.create_session username password

let test_get_author_feed _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let author_feed = Feed.get_author_feed test_session "david-engelmann.bsky.social" 20 in
  Printf.printf "Author Feed: %s\n" author_feed;
  OUnit2.assert_bool "Author Feed is not empty" (author_feed <> "")

let test_get_likes _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let likes = Feed.get_likes test_session "at://did:plc:xov3uvxfd4to6ev3ak5g5uxk/app.bsky.feed.post/3jyf6gx25eb27" "bafyreiarimgpoqvxxnf3sg4h52gvfzvmyeybxk2xgy6v3dra7zuldy73aq" 1 in
  Printf.printf "Likes Feed: %s\n" likes;
  OUnit2.assert_bool "Likes Feed is not empty" (likes <> "")

let test_get_post_thread _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let post_thread = Feed.get_post_thread test_session "at://did:plc:xov3uvxfd4to6ev3ak5g5uxk/app.bsky.feed.post/3jyf6gx25eb27" 1 in
  Printf.printf "Post Thread Feed: %s\n" post_thread;
  OUnit2.assert_bool "Post Thread Feed is not empty" (post_thread <> "")

let test_get_posts _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let posts = Feed.get_posts test_session ["at://did:plc:xov3uvxfd4to6ev3ak5g5uxk/app.bsky.feed.post/3jyf6gx25eb27"; "at://did:plc:h3lbzrp2qum5nyzpeq6anmty/app.bsky.feed.post/3jyh24qvwwt2s"] in
  Printf.printf "Posts Feed: %s\n" posts;
  OUnit2.assert_bool "Posts Feed is not empty" (posts <> "")

let test_get_reposted_by _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let reposted_by = Feed.get_reposted_by test_session "at://did:plc:xov3uvxfd4to6ev3ak5g5uxk/app.bsky.feed.post/3jxyx4hdso62e" "bafyreihui4bipokenrcj6ttannh26svviq62x7hqx3oxrmejd7qhwxbasy" 1 in
  Printf.printf "Reposted By Feed: %s\n" reposted_by;
  OUnit2.assert_bool "Reposted By Feed is not empty" (reposted_by <> "")

let test_get_timeline _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let timeline = Feed.get_timeline test_session "reverse-chronological" 2 in
  Printf.printf "Timeline Feed: %s\n" timeline;
  OUnit2.assert_bool "Timeline Feed is not empty" (timeline <> "")

(*
let test_get_feed_skeleton _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let feed_skeleton = Feed.get_feed_skeleton test_session feed 2 in
  Printf.printf "Feed Skeleton Feed: %s\n" feed_skeleton;
  OUnit2.assert_bool "Feed Skeleton Feed is not empty" (feed_skeleton <> "")
*)

let suite =
    "suite"
    >::: [
           "test_get_author_feed" >:: test_get_author_feed;
           "test_get_likes" >:: test_get_likes;
           "test_get_post_thread" >:: test_get_post_thread;
           "test_get_posts" >:: test_get_posts;
           "test_get_reposted_by" >:: test_get_reposted_by;
           "test_get_timeline" >:: test_get_timeline;
         ]

let () = run_test_tt_main suite
