open OUnit2
open Atproto.Session
open Atproto.Auth
open Atproto.Server

let create_test_session _ =
  let username, password = Auth.username_and_password_from_env in
  Session.create_session username password

let test_describe_server _ =
  skip_if
    (not Auth.has_live_credentials)
    "ATP_AUTH not configured; live Bluesky test skipped";
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let server_description = Server.describe_server test_session in
  Printf.printf "Server Description: %s\n" server_description;
  OUnit2.assert_bool "Server Description is not empty" (server_description <> "")

let test_get_account_invite_codes _ =
  skip_if
    (not Auth.has_live_credentials)
    "ATP_AUTH not configured; live Bluesky test skipped";
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let account_invite_codes =
    Server.get_account_invite_codes test_session true false
  in
  Printf.printf "Account Invite Codes: %s\n" account_invite_codes;
  OUnit2.assert_bool "Account Invite Codes is not empty"
    (account_invite_codes <> "")

let test_list_app_passwords _ =
  skip_if
    (not Auth.has_live_credentials)
    "ATP_AUTH not configured; live Bluesky test skipped";
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let app_passwords = Server.list_app_passwords test_session in
  Printf.printf "App Passwords: %s\n" app_passwords;
  OUnit2.assert_bool "App Passwords is not empty" (app_passwords <> "")

let test_parse_service_auth _ =
  let json = `Assoc [ ("token", `String "header.payload.sig") ] in
  let auth = Server.parse_service_auth json in
  OUnit2.assert_equal ~printer:(fun x -> x) "header.payload.sig" auth.token

let test_parse_account_status _ =
  let json =
    `Assoc
      [
        ("activated", `Bool true);
        ("validDid", `Bool true);
        ("expectedBlobs", `Int 3);
        ("importedBlobs", `Int 3);
      ]
  in
  let st = Server.parse_account_status json in
  OUnit2.assert_equal (Some true) st.activated;
  OUnit2.assert_equal (Some 3) st.expected_blobs

let test_account_urls _ =
  skip_if
    (not Auth.has_live_credentials)
    "ATP_AUTH not configured; live Bluesky test skipped";
  let s = create_test_session () in
  OUnit2.assert_bool "deactivate"
    (let u = Server.deactivate_account_url s in
     String.length u > 18
     && String.sub u (String.length u - 18) 18 = "deactivateAccount");
  OUnit2.assert_bool "checkAccountStatus"
    (let u = Server.check_account_status_url s in
     String.length u > 18
     && String.sub u (String.length u - 18) 18 = "checkAccountStatus")

let suite =
  "suite"
  >::: [
         "test_describe_server" >:: test_describe_server;
         "test_get_account_invite_codes" >:: test_get_account_invite_codes;
         "test_list_app_passwords" >:: test_list_app_passwords;
         "test_parse_service_auth" >:: test_parse_service_auth;
         "test_parse_account_status" >:: test_parse_account_status;
         "test_account_urls" >:: test_account_urls;
       ]

let () = run_test_tt_main suite
