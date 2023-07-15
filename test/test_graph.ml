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
  Printf.printf "Graph Blocks: %s\n" blocks;
  OUnit2.assert_bool "Graph Blocks is not empty" (blocks <> "")

let test_get_followers _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let followers = Graph.get_followers test_session "david-engelmann" 10 in
  Printf.printf "Graph Followers: %s\n" followers;
  OUnit2.assert_bool "Graph Followers is not empty" (followers <> "")


let suite =
    "suite"
    >::: [
          "test_get_blocks" >:: test_get_blocks;
          "test_get_followers" >:: test_get_followers;

         ]

let () = run_test_tt_main suite
