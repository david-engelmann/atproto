open OUnit2
open Bluesky.Session
open Bluesky.Auth

let sample_session : Session.session = {
      username = "your.name";
      password = "password";
      atp_host = "https://bsky.social";
      auth = {
        exp = 0;
        iat = 0;
        scope = "";
        did = "did_public_key";
        jti = None;
        token = "JWToken";
      };
    }

let test_sample_session_username _ =
    match sample_session with
     | { username; _ } ->
        OUnit2.assert_equal "your.name" username

let test_sample_session_password _ =
    match sample_session with
     | { password; _ } ->
        OUnit2.assert_equal "password" password

let test_sample_session_atp_host _ =
    match sample_session with
     | { atp_host; _ } ->
        OUnit2.assert_equal "https://bsky.social" atp_host

let test_sample_session_auth_token _ =
    match sample_session with
     | { auth = { token; _ }; _ } ->
        OUnit2.assert_equal "JWToken" token

let test_sample_session_auth_did _ =
    match sample_session with
     | { auth = { did; _ }; _ } ->
        OUnit2.assert_equal "did_public_key" did

let test_create_session _ =
    let (username, password) = Auth.username_and_password_from_env in
    let test_session = Session.create_session username password in
    OUnit2.assert_equal ~printer:string_of_bool true ((String.length test_session.username) > 0);
    OUnit2.assert_equal ~printer:string_of_bool true ((String.length test_session.password) > 0);
    OUnit2.assert_equal ~printer:string_of_bool true ((String.length test_session.atp_host) > 0);
    OUnit2.assert_equal ~printer:string_of_bool true ((String.length test_session.auth.token) > 0);
    OUnit2.assert_equal ~printer:string_of_bool true ((String.length test_session.auth.did) > 0)

let test_bearer_token_from_session _ =
    let (username, password) = Auth.username_and_password_from_env in
    let test_session = Session.create_session username password in
    let bearer_token = Session.bearer_token_from_session test_session in
    match bearer_token with
    | (setting_name, bearer) ->
        OUnit2.assert_equal "Authorization" setting_name;
        OUnit2.assert_equal ~printer:string_of_bool true ((String.length bearer) > 0)

let suite =
  "suite"
  >::: [
         "test_sample_session_username" >:: test_sample_session_username;
         "test_sample_session_password" >:: test_sample_session_password;
         "test_sample_session_atp_host" >:: test_sample_session_atp_host;
         "test_sample_session_auth_token" >:: test_sample_session_auth_token;
         "test_sample_session_auth_did" >:: test_sample_session_auth_did;
         "test_create_session" >:: test_create_session;
         "test_bearer_token_from_session" >:: test_bearer_token_from_session;
       ]

let () =
    run_test_tt_main suite
