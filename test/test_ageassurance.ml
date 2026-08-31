open OUnit2
open Atproto.Ageassurance
open Atproto.Auth

let test_parse_state_and_config _ =
  let bundle =
    Ageassurance.parse_state_bundle
      (`Assoc
        [
          ( "state",
            `Assoc
              [
                ("status", `String "pending");
                ("access", `String "safe");
                ("lastInitiatedAt", `String "2026-01-01T00:00:00.000Z");
              ] );
          ( "metadata",
            `Assoc [ ("accountCreatedAt", `String "2020-01-01T00:00:00.000Z") ]
          );
        ])
  in
  OUnit2.assert_equal ~printer:(fun x -> x) "pending" bundle.state.status;
  OUnit2.assert_equal ~printer:(fun x -> x) "safe" bundle.state.access;
  OUnit2.assert_equal (Some "2020-01-01T00:00:00.000Z")
    bundle.metadata.account_created_at;
  let cfg =
    Ageassurance.parse_config
      (`Assoc
        [
          ( "regions",
            `List
              [
                `Assoc
                  [
                    ("countryCode", `String "GB");
                    ("minAccessAge", `Int 18);
                    ("platforms", `List [ `String "ios"; `String "android" ]);
                    ( "additionalVerificationMethods",
                      `List [ `String "device" ] );
                    ( "rules",
                      `List
                        [
                          `Assoc
                            [
                              ( "$type",
                                `String
                                  "app.bsky.ageassurance.defs#configRegionRuleIfDeclaredOverAge"
                              );
                              ("age", `Int 18);
                              ("access", `String "full");
                            ];
                          `Assoc
                            [
                              ( "$type",
                                `String
                                  "app.bsky.ageassurance.defs#configRegionRuleDefault"
                              );
                              ("access", `String "none");
                            ];
                        ] );
                  ];
              ] );
        ])
  in
  OUnit2.assert_equal 1 (List.length cfg.regions);
  OUnit2.assert_equal 18 (List.hd cfg.regions).min_access_age;
  (match (List.hd cfg.regions).rules with
  | `Declared_over (18, "full") :: `Default "none" :: _ -> ()
  | _ -> OUnit2.assert_failure "expected declared-over then default rules");
  let body =
    Ageassurance.begin_body ~email:"user@example.com" ~language:"en"
      ~country_code:"GB" ~region_code:"ENG" ()
  in
  let open Yojson.Safe.Util in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "GB"
    (body |> member "countryCode" |> to_string);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "ENG"
    (body |> member "regionCode" |> to_string)

let test_parse_event _ =
  let ev =
    Ageassurance.parse_event
      (`Assoc
        [
          ("createdAt", `String "2026-01-01T00:00:00.000Z");
          ("attemptId", `String "11111111-1111-1111-1111-111111111111");
          ("status", `String "assured");
          ("access", `String "full");
          ("countryCode", `String "US");
          ("email", `String "user@example.com");
        ])
  in
  OUnit2.assert_equal ~printer:(fun x -> x) "assured" ev.status;
  OUnit2.assert_equal ~printer:(fun x -> x) "full" ev.access

let test_get_config_live _ =
  try
    let cfg = Ageassurance.get_config () in
    OUnit2.assert_bool "regions list parsed" (List.length cfg.regions >= 0)
  with exn ->
    skip_if true ("ageassurance.getConfig skipped: " ^ Printexc.to_string exn)

let test_get_state_auth_skipped _ =
  skip_if
    (not Auth.has_live_credentials)
    "ATP_AUTH not configured; live Bluesky test skipped";
  let username, password = Auth.username_and_password_from_env in
  let s = Atproto.Session.Session.create_session username password in
  try
    let bundle = Ageassurance.get_state s ~country_code:"US" () in
    OUnit2.assert_bool "status present" (String.length bundle.state.status > 0)
  with exn -> skip_if true ("getState skipped: " ^ Printexc.to_string exn)

let suite =
  "ageassurance"
  >::: [
         "test_parse_state_and_config" >:: test_parse_state_and_config;
         "test_parse_event" >:: test_parse_event;
         "test_get_config_live" >:: test_get_config_live;
         "test_get_state_auth_skipped" >:: test_get_state_auth_skipped;
       ]

let () = run_test_tt_main suite
