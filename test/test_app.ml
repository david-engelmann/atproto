open OUnit2
open Bluesky.Session
open Bluesky.Auth
open Bluesky.App

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


let suite =
    "suite"
    >::: [
           "test_get_profile" >:: test_get_profile;
           "test_get_profiles" >:: test_get_profiles;
         ]

let () = run_test_tt_main suite
