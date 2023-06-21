open OUnit2
open Atproto.Session
open Atproto.Auth
open Atproto.Label

let create_test_session _ =
    let (username, password) = Auth.username_and_password_from_env in
    Session.create_session username password



