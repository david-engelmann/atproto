open OUnit2
open Bluesky.Session

let sample_session : Session.session = {
      username = "your.name";
      password = "password";
      atp_host = "https://bsky.social";
      atp_auth_token = "JWToken"; (* set after login *)
      did = "did_public_key"; (* set after login *)
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

let test_sample_session_atp_auth_token _ =
    match sample_session with
     | { atp_auth_token; _ } ->
        OUnit2.assert_equal "JWToken" atp_auth_token

let test_sample_session_did _ =
    match sample_session with
     | { did; _ } ->
        OUnit2.assert_equal "did_public_key" did

let test_create_session _ =
    let test_session = Session.create_session "david.engelmann44@gmail.com" "lsnv-tc3a-7wrl-upct" in
    OUnit2.assert_bool "username is a string" (String.length test_session.username > 0);
    OUnit2.assert_bool "password is a string" (String.length test_session.password > 0);
    OUnit2.assert_bool "atp_host is a string" (String.length test_session.atp_host > 0);
    OUnit2.assert_bool "atp_auth_token is a string" (String.length test_session.atp_auth_token > 0);
    OUnit2.assert_bool "did is a string" (String.length test_session.did > 0);

let suite =
  "suite"
  >::: [
         "test_sample_session_username" >:: test_sample_session_username;
         "test_sample_session_password" >:: test_sample_session_password;
         "test_sample_session_atp_host" >:: test_sample_session_atp_host;
         "test_sample_session_atp_auth_token" >:: test_sample_session_atp_auth_token;
         "test_sample_session_did" >:: test_sample_session_did;
         "test_create_session" >:: test_create_session;
       ]

let () = run_test_tt_main suite
