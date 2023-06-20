open OUnit2
open Atproto.Session
open Atproto.Auth
open Atproto.App

let test_get_profile _ =
  let (username, password) = Auth.username_and_password_from_env in
  let test_session = Session.create_session username password in
  let profile = App.get_profile test_session "david-engelmann.bsky.social" in
  Printf.printf "Profile: %s\n" profile;
  OUnit2.assert_bool "Profile is not empty" (profile <> "")

let test_get_profiles _ =
  let (username, password) = Auth.username_and_password_from_env in
  let test_session = Session.create_session username password in
  let profiles = App.get_profiles test_session ["david-engelmann.bsky.social"; "jay.bsky.team"] in
  Printf.printf "Profiles: %s\n" profiles;
  OUnit2.assert_bool "Profiles is not empty" (profiles <> "")

let test_get_suggestions _ =
  let (username, password) = Auth.username_and_password_from_env in
  let test_session = Session.create_session username password in
  let suggestions = App.get_suggestions test_session 5 in
  Printf.printf "Suggestions: %s\n" suggestions;
  OUnit2.assert_bool "Suggestions is not empty" (suggestions <> "")

let test_search_actors _ =
  let (username, password) = Auth.username_and_password_from_env in
  let test_session = Session.create_session username password in
  let profiles = App.search_actors test_session "david-engelmann" 1 in
  Printf.printf "Search Profiles: %s\n" profiles;
  OUnit2.assert_bool "Search Profiles is not empty" (profiles <> "")

let test_search_actors_typeahead _ =
  let (username, password) = Auth.username_and_password_from_env in
  let test_session = Session.create_session username password in
  let profiles = App.search_actors_typeahead test_session "david-engelmann" 1 in
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
