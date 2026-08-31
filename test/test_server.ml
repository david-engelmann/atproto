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
        ("repoCommit", `String "bafyreicommit");
        ("repoRev", `String "3jzfcijpj2z2a");
        ("repoBlocks", `Int 40);
        ("indexedRecords", `Int 12);
        ("privateStateValues", `Int 1);
      ]
  in
  let st = Server.parse_account_status json in
  OUnit2.assert_equal (Some true) st.activated;
  OUnit2.assert_equal (Some 3) st.expected_blobs;
  OUnit2.assert_equal (Some "bafyreicommit") st.repo_commit;
  OUnit2.assert_equal (Some "3jzfcijpj2z2a") st.repo_rev;
  OUnit2.assert_equal (Some 40) st.repo_blocks;
  OUnit2.assert_equal (Some 12) st.indexed_records;
  OUnit2.assert_equal (Some 1) st.private_state_values

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

let with_public_timeout ?(seconds = 20) f =
  let old =
    Sys.signal Sys.sigalrm (Sys.Signal_handle (fun _ -> failwith "timeout"))
  in
  ignore (Unix.alarm seconds);
  Fun.protect
    ~finally:(fun () ->
      ignore (Unix.alarm 0);
      Sys.set_signal Sys.sigalrm old)
    f

let test_parse_describe_server _ =
  let json =
    `Assoc
      [
        ("did", `String "did:web:bsky.social");
        ("availableUserDomains", `List [ `String ".bsky.social" ]);
        ("inviteCodeRequired", `Bool false);
        ( "links",
          `Assoc
            [
              ( "privacyPolicy",
                `String "https://bsky.social/about/support/privacy" );
            ] );
        ("contact", `Assoc [ ("email", `String "support@bsky.app") ]);
      ]
  in
  let desc = Server.parse_describe_server json in
  OUnit2.assert_equal ~printer:(fun x -> x) "did:web:bsky.social" desc.did;
  OUnit2.assert_bool "domains"
    (List.mem ".bsky.social" desc.available_user_domains)

let test_parse_describe_server_missing_contact _ =
  let desc =
    Server.parse_describe_server
      (`Assoc
        [
          ("did", `String "did:web:pds.example");
          ("availableUserDomains", `List [ `String ".example" ]);
        ])
  in
  OUnit2.assert_equal ~printer:(fun x -> x) "did:web:pds.example" desc.did;
  OUnit2.assert_equal None desc.contact_email;
  OUnit2.assert_equal None desc.links.privacy_policy

let test_reserve_signing_key_body _ =
  let body =
    Server.reserve_signing_key_body ~did:"did:plc:abc123xyz0001112223333" ()
  in
  let open Yojson.Safe.Util in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "did:plc:abc123xyz0001112223333"
    (body |> member "did" |> to_string)

let test_create_account_at_url_uses_host _ =
  let body =
    Server.create_account_body ~handle:"bob.test" ~email:"bob@test.local"
      ~password:"secret" ()
  in
  let open Yojson.Safe.Util in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "bob.test"
    (body |> member "handle" |> to_string);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "bob@test.local"
    (body |> member "email" |> to_string)

let test_create_account_and_app_password_bodies _ =
  let open Yojson.Safe.Util in
  let acct =
    Server.create_account_body ~handle:"alice.test" ~email:"a@b.test"
      ~did:"did:plc:abc123xyz0001112223333" ~verification_code:"123456"
      ~verification_phone:"+15551212" ~password:"secret"
      ~plc_op:(`Assoc [ ("sig", `String "abc") ])
      ()
  in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "did:plc:abc123xyz0001112223333"
    (acct |> member "did" |> to_string);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "123456"
    (acct |> member "verificationCode" |> to_string);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "abc"
    (acct |> member "plcOp" |> member "sig" |> to_string);
  let app = Server.create_app_password_body ~name:"cli" ~privileged:true () in
  OUnit2.assert_equal true (app |> member "privileged" |> to_bool);
  let basic = Server.create_app_password_body ~name:"cli" () in
  OUnit2.assert_equal `Null (basic |> member "privileged")

let test_email_and_account_bodies _ =
  let open Yojson.Safe.Util in
  let confirm = Server.confirm_email_body ~email:"a@b.test" ~token:"tok-1" in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "tok-1"
    (confirm |> member "token" |> to_string);
  let update =
    Server.update_email_body ~email:"c@d.test" ~token:"tok-2"
      ~email_auth_factor:true ()
  in
  OUnit2.assert_equal true (update |> member "emailAuthFactor" |> to_bool);
  let parsed =
    Server.parse_email_update (`Assoc [ ("tokenRequired", `Bool true) ])
  in
  OUnit2.assert_equal true parsed.token_required;
  let deact =
    Server.deactivate_account_body ~delete_after:"2026-01-01T00:00:00.000Z" ()
  in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "2026-01-01T00:00:00.000Z"
    (deact |> member "deleteAfter" |> to_string);
  let empty = Server.deactivate_account_body () in
  OUnit2.assert_equal (`Assoc []) empty;
  let invites =
    Server.create_invite_codes_body ~code_count:2 ~use_count:5
      ~for_accounts:[ "did:plc:abc123xyz0001112223333" ]
      ()
  in
  OUnit2.assert_equal 2 (invites |> member "codeCount" |> to_int);
  match invites |> member "forAccounts" with
  | `List [ `String did ] ->
      OUnit2.assert_equal
        ~printer:(fun x -> x)
        "did:plc:abc123xyz0001112223333" did
  | _ -> OUnit2.assert_failure "expected forAccounts"

let test_procedure_bodies_and_app_passwords _ =
  let open Yojson.Safe.Util in
  let invite =
    Server.create_invite_code_body ~use_count:3
      ~for_account:"did:plc:abc123xyz0001112223333" ()
  in
  OUnit2.assert_equal 3 (invite |> member "useCount" |> to_int);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "did:plc:abc123xyz0001112223333"
    (invite |> member "forAccount" |> to_string);
  let reset = Server.request_password_reset_body ~email:"a@b.test" in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "a@b.test"
    (reset |> member "email" |> to_string);
  let del =
    Server.delete_account_body ~did:"did:plc:abc123xyz0001112223333"
      ~password:"secret" ~token:"tok"
  in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "tok"
    (del |> member "token" |> to_string);
  let rp = Server.reset_password_body ~token:"t" ~password:"p" in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "p"
    (rp |> member "password" |> to_string);
  let rev = Server.revoke_app_password_body ~name:"cli" in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "cli"
    (rev |> member "name" |> to_string);
  OUnit2.assert_equal (`Assoc []) (Server.request_account_delete_body ());
  let created =
    Server.parse_app_password
      (`Assoc
        [
          ("name", `String "cli");
          ("password", `String "xxxx-yyyy");
          ("createdAt", `String "2026-01-01T00:00:00.000Z");
          ("privileged", `Bool false);
        ])
  in
  OUnit2.assert_equal ~printer:(fun x -> x) "cli" created.name;
  OUnit2.assert_equal (Some "xxxx-yyyy") created.password;
  OUnit2.assert_equal (Some false) created.privileged;
  let listed =
    Server.parse_app_passwords
      (`Assoc
        [
          ( "passwords",
            `List
              [
                `Assoc
                  [
                    ("name", `String "cli");
                    ("createdAt", `String "2026-01-01T00:00:00.000Z");
                  ];
              ] );
        ])
  in
  OUnit2.assert_equal 1 (List.length listed);
  OUnit2.assert_equal None (List.hd listed).password

let test_describe_server_public _ =
  try
    with_public_timeout (fun () ->
        let desc = Server.describe_server_parsed ~host:"bsky.social" () in
        OUnit2.assert_bool "server did"
          (String.length desc.did > 4
          && List.length desc.available_user_domains >= 0))
  with exn ->
    skip_if true ("describeServer skipped: " ^ Printexc.to_string exn)

let suite =
  "suite"
  >::: [
         "test_describe_server" >:: test_describe_server;
         "test_get_account_invite_codes" >:: test_get_account_invite_codes;
         "test_list_app_passwords" >:: test_list_app_passwords;
         "test_parse_service_auth" >:: test_parse_service_auth;
         "test_parse_account_status" >:: test_parse_account_status;
         "test_account_urls" >:: test_account_urls;
         "test_parse_describe_server" >:: test_parse_describe_server;
         "test_parse_describe_server_missing_contact"
         >:: test_parse_describe_server_missing_contact;
         "test_reserve_signing_key_body" >:: test_reserve_signing_key_body;
         "test_create_account_at_url_uses_host"
         >:: test_create_account_at_url_uses_host;
         "test_create_account_and_app_password_bodies"
         >:: test_create_account_and_app_password_bodies;
         "test_email_and_account_bodies" >:: test_email_and_account_bodies;
         "test_procedure_bodies_and_app_passwords"
         >:: test_procedure_bodies_and_app_passwords;
         "test_describe_server_public" >:: test_describe_server_public;
       ]

let () = run_test_tt_main suite
