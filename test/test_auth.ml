open OUnit2
open Bluesky.Auth


let sample_auth_without_jti : Auth.auth = {
    exp = 1686612561;
    iat = 1686611561;
    scope = "read write delete";
    did = "123";
    jti = None;
  }

let sample_auth_with_jti : Auth.auth = {
    exp = 1686612561;
    iat = 1686611561;
    scope = "read write delete";
    did = "321";
    jti = Some "jti";
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

let test_make_auth_token_request_valid_info _ =
  let body = Auth.make_auth_token_request "david.engelmann44@gmail.com" "lsnv-tc3a-7wrl-upct" "bsky.social" in
  print_endline body;
  OUnit2.assert_bool "Body is not empty" (body <> "")

let test_parse_auth _ =
  let body = Auth.make_auth_token_request "david.engelmann44@gmail.com" "lsnv-tc3a-7wrl-upct" "bsky.social" in
  let test_auth = Auth.parse_auth (Auth.convert_body_to_json body) in
  assert_bool "exp is a positive integer" (auth.exp > 0);
  assert_bool "iat is a positive integer" (auth.iat > 0);
  assert_bool "scope is a non-empty string" (String.length auth.scope > 0);
  assert_bool "did is a non-empty string" (String.length auth.did > 0);
  match auth.jti with
   | Some s -> assert_bool "jti is a non-empty string" (String.length s > 0)
   | None -> assert_bool "jti is None" true



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
       ]

let () = run_test_tt_main suite
