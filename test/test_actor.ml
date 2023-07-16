open OUnit2
open Atproto.Session
open Atproto.Auth
open Atproto.Actor

let create_test_session _ =
    let (username, password) = Auth.username_and_password_from_env in
    Session.create_session username password

let test_get_profile _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let profile = Actor.get_profile test_session "david-engelmann.bsky.social" in
  match profile with
  | { handle; _ } ->
    OUnit2.assert_equal "david-engelmann.bsky.social" handle

let test_get_profiles _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let profiles = Actor.get_profiles test_session ["david-engelmann.bsky.social"; "jay.bsky.team"] in
  OUnit2.assert_equal ~printer:string_of_int (List.length profiles) 2

let test_get_suggestions _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let suggestions = Actor.get_suggestions test_session 5 in
  Printf.printf "Suggestions: %s\n" suggestions;
  OUnit2.assert_bool "Suggestions is not empty" (suggestions <> "")

let test_search_actors _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let profiles = Actor.search_actors test_session "david-engelmann" 1 in
  Printf.printf "Search Profiles: %s\n" profiles;
  OUnit2.assert_bool "Search Profiles is not empty" (profiles <> "")

let test_search_actors_typeahead _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let profiles = Actor.search_actors_typeahead test_session "david-engelmann" 1 in
  Printf.printf "Search Profiles Typeahead: %s\n" profiles;
  OUnit2.assert_bool "Search Profiles Typeahead is not empty" (profiles <> "")

let suite =
    "suite"
    >::: [
           "test_get_profile" >:: test_get_profile;
           "test_get_profiles" >:: test_get_profiles;
           "test_get_suggestions" >:: test_get_suggestions;
           "test_search_actors" >:: test_search_actors;
           "test_search_actors_typeahead" >:: test_search_actors_typeahead;
         ]

let () = run_test_tt_main suite
