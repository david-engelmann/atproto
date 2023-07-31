open OUnit2
open Atproto.Session
open Atproto.Auth
open Atproto.Graph

let create_test_session _ =
    let (username, password) = Auth.username_and_password_from_env in
    Session.create_session username password

let test_get_blocks _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let blocks = Graph.get_blocks test_session 10 in
  Printf.printf "\n\n\nGraph Blocks: %s\n\n\n" blocks;
  OUnit2.assert_bool "Graph Blocks is empty" (blocks <> "")

let test_get_followers _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let followers = Graph.get_followers test_session "david-engelmann.bsky.social" 10 in
  Printf.printf "Graph Followers: %s\n" followers;
  OUnit2.assert_bool "Graph Followers is not empty" (followers <> "")

let test_get_follows _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let follows = Graph.get_follows test_session "david-engelmann.bsky.social" 10 in
  Printf.printf "Graph Follows: %s\n" follows;
  OUnit2.assert_bool "Graph Follows is not empty" (follows <> "")

let test_get_mutes _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let mutes = Graph.get_mutes test_session 10 in
  Printf.printf "Graph Mutes: %s\n" mutes;
  OUnit2.assert_bool "Graph Mutes is not empty" (mutes <> "")

let test_mute_actor _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let muted_actor = Graph.mute_actor test_session "karen.bsky.social" in
  Printf.printf "Graph Mute Actor: %s\n" muted_actor;
  OUnit2.assert_bool "Graph Mute Actor is not empty" (muted_actor = "")

let test_unmute_actor _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let unmuted_actor = Graph.unmute_actor test_session "karen.bsky.social" in
  Printf.printf "Graph Unmute Actor: %s\n" unmuted_actor;
  OUnit2.assert_bool "Graph Unmute Actor is not empty" (unmuted_actor = "")

let suite =
    "suite"
    >::: [
          "test_get_blocks" >:: test_get_blocks;
          "test_get_followers" >:: test_get_followers;
          "test_get_follows" >:: test_get_follows;
          "test_get_mutes" >:: test_get_mutes;
          "test_mute_actor" >:: test_mute_actor;
          "test_unmute_actor" >:: test_unmute_actor;


         ]

let () = run_test_tt_main suite
