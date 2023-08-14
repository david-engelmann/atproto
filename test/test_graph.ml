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
  match blocks with
  | { blocks; _ } ->
    match blocks with
    | [] -> OUnit2.assert_bool "Graph blocks is completely empty" ( 1 < 0 )
    | hd :: _ ->
      match hd with
      | {did; _} -> OUnit2.assert_bool "Graph Blocks is empty" ((String.length did) > 0)

let test_get_followers _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let followers = Graph.get_followers test_session "david-engelmann.bsky.social" 10 in
  match followers with
  | { subject; _ } ->
    match subject with
    | { handle; _ } ->
      OUnit2.assert_equal "david-engelmann.bsky.social" handle

let test_get_follows _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let follows = Graph.get_follows test_session "david-engelmann.bsky.social" 10 in
  match follows with
  | { subject; _ } ->
    match subject with
    | { handle; _ } ->
      OUnit2.assert_equal "david-engelmann.bsky.social" handle

let test_get_mutes _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let mutes = Graph.get_mutes test_session 10 in
  match mutes with
  | { mutes; _ } ->
    match mutes with
    | [] -> OUnit2.assert_bool "Graph blocks is completely empty" ( 1 < 0 )
    | hd :: _ ->
      match hd with
      | {did; _} -> OUnit2.assert_bool "Graph Blocks is empty" ((String.length did) > 0)

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
