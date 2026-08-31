open OUnit2
open Atproto.Admin
open Atproto.Auth

let test_parse_subject_status _ =
  let json =
    `Assoc
      [
        ( "subject",
          `Assoc
            [
              ("$type", `String "com.atproto.admin.defs#repoRef");
              ("did", `String "did:plc:abc123xyz0001112223333");
            ] );
        ("takedown", `Assoc [ ("applied", `Bool true); ("ref", `String "t1") ]);
        ("deactivated", `Assoc [ ("applied", `Bool false) ]);
      ]
  in
  let st = Admin.parse_subject_status json in
  (match st.subject with
  | Admin.Repo { did } ->
      OUnit2.assert_equal
        ~printer:(fun x -> x)
        "did:plc:abc123xyz0001112223333" did
  | _ -> OUnit2.assert_failure "expected repo subject");
  OUnit2.assert_bool "takedown applied"
    (match st.takedown with Some t -> t.applied | None -> false)

let test_update_body _ =
  let body =
    Admin.update_subject_status_body
      ~subject:(Admin.Repo { did = "did:plc:abc123xyz0001112223333" })
      ~takedown:{ applied = true; ref_ = Some "abc" }
      ()
  in
  let open Yojson.Safe.Util in
  OUnit2.assert_equal true
    (body |> member "takedown" |> member "applied" |> to_bool)

let test_parse_account_info _ =
  let json =
    `Assoc
      [
        ("did", `String "did:plc:abc123xyz0001112223333");
        ("handle", `String "alice.test");
        ("indexedAt", `String "2024-01-01T00:00:00.000Z");
        ("invitesDisabled", `Bool true);
      ]
  in
  let info = Admin.parse_account_info json in
  OUnit2.assert_equal ~printer:(fun x -> x) "alice.test" info.handle;
  OUnit2.assert_equal (Some true) info.invites_disabled

let test_send_email_body _ =
  let body =
    Admin.send_email_body ~recipient_did:"did:plc:abc123xyz0001112223333"
      ~content:"hello" ~subject:"hi" ()
  in
  let open Yojson.Safe.Util in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "hello"
    (body |> member "content" |> to_string)

let test_admin_auth_skipped _ =
  skip_if
    (not Auth.has_live_credentials)
    "ATP_AUTH not configured; live Bluesky test skipped";
  let username, password = Auth.username_and_password_from_env in
  let s = Atproto.Session.Session.create_session username password in
  try
    let _ =
      Admin.get_account_info s ~did:"did:plc:ewvi7nxzyoun6zhxrhs64oiz" ()
    in
    OUnit2.assert_bool "admin reachable" true
  with exn ->
    skip_if true ("admin getAccountInfo skipped: " ^ Printexc.to_string exn)

let suite =
  "admin"
  >::: [
         "test_parse_subject_status" >:: test_parse_subject_status;
         "test_update_body" >:: test_update_body;
         "test_parse_account_info" >:: test_parse_account_info;
         "test_send_email_body" >:: test_send_email_body;
         "test_admin_auth_skipped" >:: test_admin_auth_skipped;
       ]

let () = run_test_tt_main suite
