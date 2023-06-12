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
