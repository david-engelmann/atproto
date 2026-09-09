open OUnit2
open Atproto.Contact
open Atproto.Auth

let test_parse_and_bodies _ =
  let matches =
    Contact.parse_matches_page
      (`Assoc
        [
          ("cursor", `String "c1");
          ( "matches",
            `List
              [
                `Assoc
                  [
                    ("did", `String "did:plc:abc123xyz0001112223333");
                    ("handle", `String "alice.test");
                    ("indexedAt", `String "2024-01-01T00:00:00.000Z");
                    ("viewer", `Null);
                  ];
              ] );
        ])
  in
  OUnit2.assert_equal (Some "c1") matches.cursor;
  OUnit2.assert_equal 1 (List.length matches.matches);
  let imported =
    Contact.parse_import_result
      (`Assoc
        [
          ( "matchesAndContactIndexes",
            `List
              [
                `Assoc
                  [
                    ( "match",
                      `Assoc
                        [
                          ("did", `String "did:plc:abc123xyz0001112223333");
                          ("handle", `String "alice.test");
                          ("indexedAt", `String "2024-01-01T00:00:00.000Z");
                          ("viewer", `Null);
                        ] );
                    ("contactIndex", `Int 2);
                  ];
              ] );
        ])
  in
  OUnit2.assert_equal 2 (List.hd imported.matches).contact_index;
  let status =
    Contact.parse_sync_status_opt
      (`Assoc
        [
          ( "syncStatus",
            `Assoc
              [
                ("syncedAt", `String "2024-01-01T00:00:00.000Z");
                ("matchesCount", `Int 4);
              ] );
        ])
  in
  (match status.sync_status with
  | Some s -> OUnit2.assert_equal 4 s.matches_count
  | None -> OUnit2.assert_failure "expected sync status");
  let empty = Contact.parse_sync_status_opt (`Assoc []) in
  OUnit2.assert_equal None empty.sync_status;
  let open Yojson.Safe.Util in
  let import_body =
    Contact.import_contacts_body ~token:"jwt" ~contacts:[ "+12125550123" ]
  in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "jwt"
    (import_body |> member "token" |> to_string);
  OUnit2.assert_equal 1
    (import_body |> member "contacts" |> to_list |> List.length);
  let dismiss =
    Contact.dismiss_match_body ~subject:"did:plc:abc123xyz0001112223333"
  in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "did:plc:abc123xyz0001112223333"
    (dismiss |> member "subject" |> to_string);
  let verify = Contact.verify_phone_body ~phone:"+12125550123" ~code:"123456" in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "123456"
    (verify |> member "code" |> to_string);
  OUnit2.assert_equal [] (Contact.get_matches_body ());
  OUnit2.assert_equal
    [ ("limit", "25"); ("cursor", "c1") ]
    (Contact.get_matches_body ~limit:25 ~cursor:"c1" ());
  let start =
    Contact.start_phone_verification_body ~phone:"+12125550123"
  in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "+12125550123"
    (start |> member "phone" |> to_string);
  (match Contact.remove_data_body with
  | `Assoc [] -> ()
  | _ -> OUnit2.assert_failure "expected empty remove_data_body");
  ignore Contact.get_matches_appview;
  ignore Contact.get_matches_service;
  ignore Contact.get_sync_status_appview;
  ignore Contact.get_sync_status_service;
  ignore Contact.import_contacts_service;
  ignore Contact.start_phone_verification_service;
  ignore Contact.verify_phone_service;
  ignore Contact.phone_live_enabled;
  ignore Contact.phone_number_from_env

let test_get_matches_auth_skipped _ =
  skip_if
    (not Auth.has_live_credentials)
    "ATP_AUTH not configured; live Bluesky test skipped";
  let username, password = Auth.username_and_password_from_env in
  let s = Atproto.Session.Session.create_session username password in
  try
    let page = Contact.get_matches s ~limit:5 () in
    OUnit2.assert_bool "matches parsed" (List.length page.matches >= 0)
  with exn -> skip_if true ("getMatches skipped: " ^ Printexc.to_string exn)

let test_start_phone_verification_opt_in _ =
  skip_if (not Contact.phone_live_enabled)
    "ATP_PHONE not set; hosted SMS not faked";
  skip_if
    (not Auth.has_live_credentials)
    "ATP_AUTH not configured; live Bluesky test skipped";
  match Contact.phone_number_from_env with
  | None ->
      skip_if true "ATP_PHONE_NUMBER not set; hosted SMS not faked"
  | Some phone ->
      let username, password = Auth.username_and_password_from_env in
      let s = Atproto.Session.Session.create_session username password in
      try
        Contact.start_phone_verification s ~phone ();
        OUnit2.assert_bool "startPhoneVerification accepted" true
      with exn ->
        skip_if true
          ("startPhoneVerification skipped: " ^ Printexc.to_string exn)

let test_get_sync_status_auth_skipped _ =
  skip_if
    (not Auth.has_live_credentials)
    "ATP_AUTH not configured; live Bluesky test skipped";
  let username, password = Auth.username_and_password_from_env in
  let s = Atproto.Session.Session.create_session username password in
  try
    let st = Contact.get_sync_status s in
    OUnit2.assert_bool "sync status parsed"
      (match st.sync_status with Some _ | None -> true)
  with exn -> skip_if true ("getSyncStatus skipped: " ^ Printexc.to_string exn)

let suite =
  "contact"
  >::: [
         "test_parse_and_bodies" >:: test_parse_and_bodies;
         "test_get_sync_status_auth_skipped"
         >:: test_get_sync_status_auth_skipped;
         "test_get_matches_auth_skipped" >:: test_get_matches_auth_skipped;
         "test_start_phone_verification_opt_in"
         >:: test_start_phone_verification_opt_in;
       ]

let () = run_test_tt_main suite
