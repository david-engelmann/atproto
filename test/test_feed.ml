open OUnit2
open Atproto.Session
open Atproto.Auth
open Atproto.Feed

let create_test_session _ =
    let (username, password) = Auth.username_and_password_from_env in
    Session.create_session username password

let test_get_author_feed _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let author_feed = Feed.get_author_feed test_session "david-engelmann.bsky.social" 50 in
  OUnit2.assert_bool "Author Feed is empty" ((List.length author_feed) > 10)

let test_get_likes _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let l = Feed.get_likes test_session "at://did:plc:xov3uvxfd4to6ev3ak5g5uxk/app.bsky.feed.post/3jyf6gx25eb27" "bafyreiarimgpoqvxxnf3sg4h52gvfzvmyeybxk2xgy6v3dra7zuldy73aq" 10 in
  match l with
  | { likes; _ } ->
    OUnit2.assert_bool "Likes Feed is empty" ((List.length likes) > 0)

let test_get_post_thread _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let post_thread = Feed.get_post_thread test_session "at://did:plc:xov3uvxfd4to6ev3ak5g5uxk/app.bsky.feed.post/3jyf6gx25eb27" 1 in
  match post_thread with
  | { thread; _ } ->
    match thread with
      | { thread_type; _ } ->
        OUnit2.assert_bool "Post Thread Feed is empty" (thread_type <> "")

let test_get_posts _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let p = Feed.get_posts test_session ["at://did:plc:xov3uvxfd4to6ev3ak5g5uxk/app.bsky.feed.post/3jyf6gx25eb27"; "at://did:plc:h3lbzrp2qum5nyzpeq6anmty/app.bsky.feed.post/3jyh24qvwwt2s"] in
  match p with
  | { posts } ->
    OUnit2.assert_bool "Posts Feed is empty" ((List.length posts) > 0)

let test_get_reposted_by _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let reposted_by = Feed.get_reposted_by test_session "at://did:plc:xov3uvxfd4to6ev3ak5g5uxk/app.bsky.feed.post/3jxyx4hdso62e" "bafyreihui4bipokenrcj6ttannh26svviq62x7hqx3oxrmejd7qhwxbasy" 1 in
  match reposted_by with
  | { uri; _ } ->
    OUnit2.assert_bool "Reposted By Feed Uri is empty" (uri <> "")

let test_get_timeline _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let timeline = Feed.get_timeline test_session "reverse-chronological" 2 in
  match timeline with
  | { cursor; _ } ->
    OUnit2.assert_bool "Timeline Feed is empty" (cursor <> "")

let test_get_feed_skeleton _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let feed_skeleton = Feed.get_feed_skeleton test_session "1690211207532::bafyreiczrkekkemsr7qx6robw5y2tcnwccmbuvhhv5tbhsvnuhq4ow2sve" 2 in
  Printf.printf "\n\nFeed Skeleton Feed: %s\n\n" feed_skeleton;
  OUnit2.assert_bool "Feed Skeleton Feed is not empty" (feed_skeleton <> "")

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
