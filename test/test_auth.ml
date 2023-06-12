open OUnit2
open Bluesky.Auth

let sample_auth_without_jti : Auth.auth = {
    exp = "06-13-2023 22:29:34";
    iat = "06-12-2023 22:29:34";
    scope = "read write delete";
    did = "123";
    jti = None;
  }

let sample_auth_with_jti : Auth.auth = {
    exp = "06-13-2023 22:29:34";
    iat = "06-12-2023 22:29:34";
    scope = "read write delete";
    did = "321";
    jti = Some "jti";
  }

let test_sample_auth_with_jti_exp _ =
    match sample_auth_with_jti with
     | { exp; _ } ->
        OUnit2.assert_equal "06-13-2023 22:29:34" exp

let test_sample_auth_with_jti_iat _ =
    match sample_auth_with_jti with
     | { iat; _ } ->
        OUnit2.assert_equal "06-12-2023 22:29:34" iat

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
        OUnit2.assert_equal "06-13-2023 22:29:34" exp

let test_sample_auth_without_jti_iat _ =
    match sample_auth_without_jti with
     | { iat; _ } ->
        OUnit2.assert_equal "06-12-2023 22:29:34" iat

let test_sample_auth_without_jti_scope _ =
    match sample_auth_without_jti with
     | { scope; _ } ->
        OUnit2.assert_equal "read write delete" scope

let test_sample_auth_without_jti_did _ =
    match sample_auth_without_jti with
     | { did; _ } ->
        OUnit2.assert_equal "321" did

let test_sample_auth_without_jti_jti _ =
    match sample_auth_without_jti with
     | { jti; _ } ->
      match jti with
       | Some _ ->
         OUnit2.assert_equal 0 1
       | _ ->
         OUnit2.assert_equal 1 1

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
       ]

let () = run_test_tt_main suite
