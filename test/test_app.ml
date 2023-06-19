open OUnit2
open Bluesky.Session
open Bluesky.Auth
open Bluesky.App

let test_get_profile _ =
  let (username, password) = Auth.username_and_password_from_env in
  let test_session = Session.create_session username password in
  let profile = App.get_profile test_session "david-engelmann.bsky.social" in
  Printf.printf "Profile: %d\n" profile;
  OUnit2.assert_bool "Body is not empty" (body <> "")
