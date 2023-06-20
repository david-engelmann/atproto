open OUnit2
open Atproto.Auth


let sample_auth_without_jti : Auth.auth = {
    exp = 1686612561;
    iat = 1686611561;
    scope = "read write delete";
    did = "123";
    jti = None;
    token = "eyJCI6MTY4NzAyNjg0MCwiZXhwIjoxNjg3MDM0MDQwfQ.ZQem8wFw4HdYbbAnHpSvcwB3ue9HHK37K4QJ4QOzhKE";
  }

let sample_auth_with_jti : Auth.auth = {
    exp = 1686612561;
    iat = 1686611561;
    scope = "read write delete";
    did = "321";
    jti = Some "jti";
    token = "eyJCI6MTY4NzAyNjg0MCwiZXhwIjoxNjg3MDM0MDQwfQ.ZQem8wFw4HdYbbAnHpSvcwB3ue9HHK37K4QJ4QOzhKE";
  }

let test_sample_auth_with_jti_exp _ =
    match sample_auth_with_jti with
     | { exp; _ } ->
        OUnit2.assert_equal 1686612561 exp

let test_sample_auth_with_jti_iat _ =
    match sample_auth_with_jti with
     | { iat; _ } ->
        OUnit2.assert_equal 1686611561 iat

let test_sample_auth_with_jti_scope _ =
    match sample_auth_with_jti with
     | { scope; _ } ->
        OUnit2.assert_equal "read write delete" scope

let test_sample_auth_with_jti_did _ =
    match sample_auth_with_jti with
     | { did; _ } ->
        OUnit2.assert_equal "321" did

let test_sample_auth_with_jti_jti _ =
    match sample_auth_with_jti with
     | { jti; _ } ->
      match jti with
       | Some j ->
         OUnit2.assert_equal "jti" j
       | _ ->
         OUnit2.assert_equal 0 1

let test_sample_auth_with_jti_token _ =
    match sample_auth_with_jti with
     | { token; _ } ->
        OUnit2.assert_equal "eyJCI6MTY4NzAyNjg0MCwiZXhwIjoxNjg3MDM0MDQwfQ.ZQem8wFw4HdYbbAnHpSvcwB3ue9HHK37K4QJ4QOzhKE" token

let test_sample_auth_without_jti_exp _ =
    match sample_auth_without_jti with
     | { exp; _ } ->
        OUnit2.assert_equal 1686612561 exp

let test_sample_auth_without_jti_iat _ =
    match sample_auth_without_jti with
     | { iat; _ } ->
        OUnit2.assert_equal 1686611561 iat

let test_sample_auth_without_jti_scope _ =
    match sample_auth_without_jti with
     | { scope; _ } ->
        OUnit2.assert_equal "read write delete" scope

let test_sample_auth_without_jti_did _ =
    match sample_auth_without_jti with
     | { did; _ } ->
        OUnit2.assert_equal "123" did

let test_sample_auth_without_jti_jti _ =
    match sample_auth_without_jti with
     | { jti; _ } ->
      match jti with
       | Some _ ->
         OUnit2.assert_equal 0 1
       | _ ->
         OUnit2.assert_equal 1 1

let test_sample_auth_without_jti_token _ =
    match sample_auth_without_jti with
    | { token; _ } ->
        OUnit2.assert_equal "eyJCI6MTY4NzAyNjg0MCwiZXhwIjoxNjg3MDM0MDQwfQ.ZQem8wFw4HdYbbAnHpSvcwB3ue9HHK37K4QJ4QOzhKE" token

let test_make_auth_token_request_valid_info _ =
  let (username, password) = Auth.username_and_password_from_env in
  let body = Auth.make_auth_token_request username password "bsky.social" in
  print_endline body;
  OUnit2.assert_bool "Body is not empty" (body <> "")

let test_parse_auth _ =
  let (username, password) = Auth.username_and_password_from_env in
  let body = Auth.make_auth_token_request username password "bsky.social" in
  let test_auth = Auth.parse_auth (Auth.convert_body_to_json body) in
  OUnit2.assert_equal ~printer:string_of_bool true (test_auth.exp > 0);
  OUnit2.assert_equal ~printer:string_of_bool true (test_auth.iat > 0);
  OUnit2.assert_equal ~printer:string_of_bool true ((String.length test_auth.scope) > 0);
  OUnit2.assert_equal ~printer:string_of_bool true ((String.length test_auth.did) > 0);
  OUnit2.assert_equal ~printer:string_of_bool true ((String.length test_auth.token) > 0);
  match test_auth.jti with
   | Some s -> OUnit2.assert_equal ~printer:string_of_bool true ((String.length s) > 0);
   | None -> OUnit2.assert_equal 1 0

let suite =
  "suite"
  >::: [
         "test_sample_auth_with_jti_exp" >:: test_sample_auth_with_jti_exp;
         "test_sample_auth_with_jti_iat" >:: test_sample_auth_with_jti_iat;
         "test_sample_auth_with_jti_scope" >:: test_sample_auth_with_jti_scope;
         "test_sample_auth_with_jti_did" >:: test_sample_auth_with_jti_did;
         "test_sample_auth_with_jti_jti" >:: test_sample_auth_with_jti_jti;
         "test_sample_auth_without_jti_exp" >:: test_sample_auth_without_jti_exp;
         "test_sample_auth_without_jti_iat" >:: test_sample_auth_without_jti_iat;
         "test_sample_auth_without_jti_scope" >:: test_sample_auth_without_jti_scope;
         "test_sample_auth_without_jti_did" >:: test_sample_auth_without_jti_did;
         "test_sample_auth_without_jti_jti" >:: test_sample_auth_without_jti_jti;
         "test_make_auth_token_request_valid_info" >:: test_make_auth_token_request_valid_info;
         "test_parse_auth" >:: test_parse_auth;
       ]

let () = run_test_tt_main suite
