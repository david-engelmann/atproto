open OUnit2
open Atproto.Auth

let sample_auth_without_jti : Auth.auth =
  {
    exp = 1686612561;
    iat = 1686611561;
    scope = "read write delete";
    did = "123";
    jti = None;
    token =
      "eyJCI6MTY4NzAyNjg0MCwiZXhwIjoxNjg3MDM0MDQwfQ.ZQem8wFw4HdYbbAnHpSvcwB3ue9HHK37K4QJ4QOzhKE";
    refresh_token = None;
  }

let sample_auth_with_jti : Auth.auth =
  {
    exp = 1686612561;
    iat = 1686611561;
    scope = "read write delete";
    did = "321";
    jti = Some "jti";
    token =
      "eyJCI6MTY4NzAyNjg0MCwiZXhwIjoxNjg3MDM0MDQwfQ.ZQem8wFw4HdYbbAnHpSvcwB3ue9HHK37K4QJ4QOzhKE";
    refresh_token =
      Some
        "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzY29wZSI6ImNvbS5hdHByb3RvLnJlZnJlc2giLCJzdWIiOiJkaWQ6cGxjOnhvdjN1dnhmZDR0bzZldjNhazVnNXV4ayIsImp0aSI6InM0Z2JDcWRXRlVhQ1lJQk4xdk93V2xBS01LR3ZkSnlla1V3TjJKL1paUDQiLCJpYXQiOjE2ODcyODgzMjIsImV4cCI6MTY5NTA2NDMyMn0.2wdx89mPzrwVyFHhVOpHw6iIooFCE3k6a4qvvBNwcCE";
  }

let test_sample_auth_with_jti_exp _ =
  match sample_auth_with_jti with
  | { exp; _ } -> OUnit2.assert_equal 1686612561 exp

let test_sample_auth_with_jti_iat _ =
  match sample_auth_with_jti with
  | { iat; _ } -> OUnit2.assert_equal 1686611561 iat

let test_sample_auth_with_jti_scope _ =
  match sample_auth_with_jti with
  | { scope; _ } -> OUnit2.assert_equal "read write delete" scope

let test_sample_auth_with_jti_did _ =
  match sample_auth_with_jti with { did; _ } -> OUnit2.assert_equal "321" did

let test_sample_auth_with_jti_jti _ =
  match sample_auth_with_jti with
  | { jti; _ } -> (
      match jti with
      | Some j -> OUnit2.assert_equal "jti" j
      | _ -> OUnit2.assert_equal 0 1)

let test_sample_auth_with_jti_token _ =
  match sample_auth_with_jti with
  | { token; _ } ->
      OUnit2.assert_equal
        "eyJCI6MTY4NzAyNjg0MCwiZXhwIjoxNjg3MDM0MDQwfQ.ZQem8wFw4HdYbbAnHpSvcwB3ue9HHK37K4QJ4QOzhKE"
        token

let test_sample_auth_with_jti_refresh_token _ =
  match sample_auth_with_jti with
  | { refresh_token; _ } -> (
      match refresh_token with
      | Some refresh_token ->
          OUnit2.assert_equal
            "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzY29wZSI6ImNvbS5hdHByb3RvLnJlZnJlc2giLCJzdWIiOiJkaWQ6cGxjOnhvdjN1dnhmZDR0bzZldjNhazVnNXV4ayIsImp0aSI6InM0Z2JDcWRXRlVhQ1lJQk4xdk93V2xBS01LR3ZkSnlla1V3TjJKL1paUDQiLCJpYXQiOjE2ODcyODgzMjIsImV4cCI6MTY5NTA2NDMyMn0.2wdx89mPzrwVyFHhVOpHw6iIooFCE3k6a4qvvBNwcCE"
            refresh_token
      | _ -> OUnit2.assert_equal 0 1)

let test_sample_auth_without_jti_exp _ =
  match sample_auth_without_jti with
  | { exp; _ } -> OUnit2.assert_equal 1686612561 exp

let test_sample_auth_without_jti_iat _ =
  match sample_auth_without_jti with
  | { iat; _ } -> OUnit2.assert_equal 1686611561 iat

let test_sample_auth_without_jti_scope _ =
  match sample_auth_without_jti with
  | { scope; _ } -> OUnit2.assert_equal "read write delete" scope

let test_sample_auth_without_jti_did _ =
  match sample_auth_without_jti with
  | { did; _ } -> OUnit2.assert_equal "123" did

let test_sample_auth_without_jti_jti _ =
  match sample_auth_without_jti with
  | { jti; _ } -> (
      match jti with
      | Some _ -> OUnit2.assert_equal 0 1
      | _ -> OUnit2.assert_equal 1 1)

let test_sample_auth_without_jti_token _ =
  match sample_auth_without_jti with
  | { token; _ } ->
      OUnit2.assert_equal
        "eyJCI6MTY4NzAyNjg0MCwiZXhwIjoxNjg3MDM0MDQwfQ.ZQem8wFw4HdYbbAnHpSvcwB3ue9HHK37K4QJ4QOzhKE"
        token

let test_sample_auth_without_jti_refresh_token _ =
  match sample_auth_without_jti with
  | { refresh_token; _ } -> (
      match refresh_token with
      | Some _ -> OUnit2.assert_equal 0 1
      | _ -> OUnit2.assert_equal 1 1)

let test_get_base_endpoint _ =
  let endpoint = Auth.get_base_endpoint in
  OUnit2.assert_bool "BASE_ENDPOINT should be non-empty and end with /"
    (String.length endpoint > 0 && endpoint.[String.length endpoint - 1] = '/')

let test_create_session_body _ =
  let open Yojson.Safe.Util in
  let basic =
    Auth.create_session_body ~identifier:"alice.test" ~password:"secret" ()
  in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "alice.test"
    (basic |> member "identifier" |> to_string);
  OUnit2.assert_equal `Null (basic |> member "authFactorToken");
  let extra =
    Auth.create_session_body ~identifier:"alice.test" ~password:"secret"
      ~auth_factor_token:"otp-9" ~allow_takendown:true ()
  in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "otp-9"
    (extra |> member "authFactorToken" |> to_string);
  OUnit2.assert_equal true (extra |> member "allowTakendown" |> to_bool)

let test_create_session_url _ =
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "https://bsky.social/xrpc/com.atproto.server.createSession"
    (Auth.create_session_url "bsky.social")

let test_make_auth_token_request_valid_info _ =
  skip_if
    (not Auth.has_live_credentials)
    "ATP_AUTH not configured; live Bluesky test skipped";
  let username, password = Auth.username_and_password_from_env in
  let body = Auth.make_auth_token_request username password "bsky.social" in
  OUnit2.assert_bool "createSession body is empty" (body <> "")

let test_parse_auth _ =
  skip_if
    (not Auth.has_live_credentials)
    "ATP_AUTH not configured; live Bluesky test skipped";
  let username, password = Auth.username_and_password_from_env in
  let body = Auth.make_auth_token_request username password "bsky.social" in
  let test_auth = Auth.parse_auth (Auth.convert_body_to_json body) in
  OUnit2.assert_equal ~printer:string_of_bool true (test_auth.exp > 0);
  OUnit2.assert_equal ~printer:string_of_bool true (test_auth.iat > 0);
  OUnit2.assert_equal ~printer:string_of_bool true
    (String.length test_auth.scope > 0);
  OUnit2.assert_equal ~printer:string_of_bool true
    (String.length test_auth.did > 0);
  OUnit2.assert_equal ~printer:string_of_bool true
    (String.length test_auth.token > 0);
  match test_auth.jti with
  | Some s ->
      OUnit2.assert_equal ~printer:string_of_bool true (String.length s > 0)
  | None -> OUnit2.assert_equal 1 0

let suite =
  "suite"
  >::: [
         "test_sample_auth_with_jti_exp" >:: test_sample_auth_with_jti_exp;
         "test_sample_auth_with_jti_iat" >:: test_sample_auth_with_jti_iat;
         "test_sample_auth_with_jti_scope" >:: test_sample_auth_with_jti_scope;
         "test_sample_auth_with_jti_did" >:: test_sample_auth_with_jti_did;
         "test_sample_auth_with_jti_jti" >:: test_sample_auth_with_jti_jti;
         "test_sample_auth_with_jti_refresh_token"
         >:: test_sample_auth_with_jti_refresh_token;
         "test_sample_auth_without_jti_exp" >:: test_sample_auth_without_jti_exp;
         "test_sample_auth_without_jti_iat" >:: test_sample_auth_without_jti_iat;
         "test_sample_auth_without_jti_scope"
         >:: test_sample_auth_without_jti_scope;
         "test_sample_auth_without_jti_did" >:: test_sample_auth_without_jti_did;
         "test_sample_auth_without_jti_jti" >:: test_sample_auth_without_jti_jti;
         "test_sample_auth_without_jti_refresh_token"
         >:: test_sample_auth_without_jti_refresh_token;
         "test_get_base_endpoint" >:: test_get_base_endpoint;
         "test_create_session_body" >:: test_create_session_body;
         "test_create_session_url" >:: test_create_session_url;
         "test_make_auth_token_request_valid_info"
         >:: test_make_auth_token_request_valid_info;
         "test_parse_auth" >:: test_parse_auth;
       ]

let () = run_test_tt_main suite
