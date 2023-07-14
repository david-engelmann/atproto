open OUnit2
open Atproto.Session
open Atproto.Auth

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
        refresh_token = Some "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzY29wZSI6ImNvbS5hdHByb3RvLnJlZnJlc2giLCJzdWIiOiJkaWQ6cGxjOnhvdjN1dnhmZDR0bzZldjNhazVnNXV4ayIsImp0aSI6InM0Z2JDcWRXRlVhQ1lJQk4xdk93V2xBS01LR3ZkSnlla1V3TjJKL1paUDQiLCJpYXQiOjE2ODcyODgzMjIsImV4cCI6MTY5NTA2NDMyMn0.2wdx89mPzrwVyFHhVOpHw6iIooFCE3k6a4qvvBNwcCE";
      };
    }

let create_test_session _ =
    let (username, password) = Auth.username_and_password_from_env in
    Session.create_session username password

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

let test_sample_session_auth_refresh_token _ =
    match sample_session with
     | { auth = { refresh_token; _ }; _ } ->
        OUnit2.assert_equal "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzY29wZSI6ImNvbS5hdHByb3RvLnJlZnJlc2giLCJzdWIiOiJkaWQ6cGxjOnhvdjN1dnhmZDR0bzZldjNhazVnNXV4ayIsImp0aSI6InM0Z2JDcWRXRlVhQ1lJQk4xdk93V2xBS01LR3ZkSnlla1V3TjJKL1paUDQiLCJpYXQiOjE2ODcyODgzMjIsImV4cCI6MTY5NTA2NDMyMn0.2wdx89mPzrwVyFHhVOpHw6iIooFCE3k6a4qvvBNwcCE" (Option.get refresh_token)

let test_sample_session_auth_did _ =
    match sample_session with
     | { auth = { did; _ }; _ } ->
        OUnit2.assert_equal "did_public_key" did

let test_create_session _ =
    let test_session = create_test_session () |> Session.refresh_session_auth in
    OUnit2.assert_equal ~printer:string_of_bool true ((String.length test_session.username) > 0);
    OUnit2.assert_equal ~printer:string_of_bool true ((String.length test_session.password) > 0);
    OUnit2.assert_equal ~printer:string_of_bool true ((String.length test_session.atp_host) > 0);
    OUnit2.assert_equal ~printer:string_of_bool true ((String.length test_session.auth.token) > 0);
    OUnit2.assert_equal ~printer:string_of_bool true ((String.length test_session.auth.did) > 0)

let test_bearer_token_from_session _ =
    let test_session = create_test_session () |> Session.refresh_session_auth in
    let bearer_token = Session.bearer_token_from_session test_session in
    match bearer_token with
    | (setting_name, bearer) ->
        OUnit2.assert_equal "Authorization" setting_name;
        OUnit2.assert_equal ~printer:string_of_bool true ((String.length bearer) > 0)

let test_get_session_request _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let session_info = Session.get_session_request test_session in
  print_endline session_info;
  OUnit2.assert_equal ~printer:string_of_bool true ((String.length session_info) > 0)

let test_delete_session _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let deleted_session = Session.delete_session test_session in
  Printf.printf "Delete Session: %s\n" deleted_session;
  OUnit2.assert_bool "Delete Session is not empty" (deleted_session <> "")



let suite =
  "suite"
  >::: [
         "test_sample_session_username" >:: test_sample_session_username;
         "test_sample_session_password" >:: test_sample_session_password;
         "test_sample_session_atp_host" >:: test_sample_session_atp_host;
         "test_sample_session_auth_token" >:: test_sample_session_auth_token;
         "test_sample_session_auth_refresh_token" >:: test_sample_session_auth_refresh_token;
         "test_sample_session_auth_did" >:: test_sample_session_auth_did;
         "test_create_session" >:: test_create_session;
         "test_bearer_token_from_session" >:: test_bearer_token_from_session;
         "test_get_session_request" >:: test_get_session_request;
         "test_delete_session" >:: test_delete_session;
       ]

let () =
    run_test_tt_main suite
