open OUnit2
open Atproto.Session
open Atproto.Auth
open Atproto.Actor

let create_test_session _ =
  let username, password = Auth.username_and_password_from_env in
  Session.create_session username password

let test_get_profile _ =
  skip_if
    (not Auth.has_live_credentials)
    "ATP_AUTH not configured; live Bluesky test skipped";
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let profile = Actor.get_profile test_session "david-engelmann.bsky.social" in
  match profile with
  | { handle; _ } -> OUnit2.assert_equal "david-engelmann.bsky.social" handle

let test_get_profiles _ =
  skip_if
    (not Auth.has_live_credentials)
    "ATP_AUTH not configured; live Bluesky test skipped";
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let profiles =
    Actor.get_profiles test_session
      [ "david-engelmann.bsky.social"; "jay.bsky.team" ]
  in
  OUnit2.assert_equal ~printer:string_of_int (List.length profiles) 2

let test_get_suggestions _ =
  skip_if
    (not Auth.has_live_credentials)
    "ATP_AUTH not configured; live Bluesky test skipped";
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let suggestions = Actor.get_suggestions test_session 5 in
  OUnit2.assert_equal ~printer:string_of_int (List.length suggestions) 5

let test_search_actors _ =
  skip_if
    (not Auth.has_live_credentials)
    "ATP_AUTH not configured; live Bluesky test skipped";
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let profiles = Actor.search_actors test_session "david-engelmann" 1 in
  match profiles with
  | [] -> OUnit2.assert_equal "blah" ""
  | hd :: _ -> (
      match hd with
      | { handle; _ } ->
          OUnit2.assert_equal "david-engelmann.bsky.social" handle)

let test_search_actors_typeahead _ =
  skip_if
    (not Auth.has_live_credentials)
    "ATP_AUTH not configured; live Bluesky test skipped";
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let profiles =
    Actor.search_actors_typeahead test_session "david-engelmann" 1
  in
  match profiles with
  | [] -> OUnit2.assert_equal "blah" ""
  | hd :: _ -> (
      match hd with
      | { handle; _ } ->
          OUnit2.assert_equal "david-engelmann.bsky.social" handle)

let test_parse_preferences _ =
  let json =
    `Assoc
      [
        ( "preferences",
          `List
            [
              `Assoc
                [
                  ("$type", `String "app.bsky.actor.defs#adultContentPref");
                  ("enabled", `Bool false);
                ];
              `Assoc
                [
                  ("$type", `String "app.bsky.actor.defs#savedFeedsPrefV2");
                  ("items", `List []);
                ];
              `Assoc
                [
                  ("$type", `String "app.bsky.actor.defs#savedFeedsPref");
                  ( "pinned",
                    `List
                      [
                        `String
                          "at://did:plc:z72i7hdynmk6r22z27h6tvur/app.bsky.feed.generator/whats-hot";
                      ] );
                  ("saved", `List []);
                  ("timelineIndex", `Int 0);
                ];
              `Assoc
                [
                  ("$type", `String "app.bsky.actor.defs#personalDetailsPref");
                  ("birthDate", `String "2000-01-01T00:00:00.000Z");
                ];
              `Assoc
                [
                  ("$type", `String "app.bsky.actor.defs#declaredAgePref");
                  ("isOverAge18", `Bool true);
                ];
              `Assoc
                [
                  ("$type", `String "app.bsky.actor.defs#feedViewPref");
                  ("feed", `String "home");
                  ("hideReplies", `Bool true);
                ];
              `Assoc
                [
                  ("$type", `String "app.bsky.actor.defs#threadViewPref");
                  ("sort", `String "hotness");
                ];
              `Assoc
                [
                  ("$type", `String "app.bsky.actor.defs#mutedWordsPref");
                  ( "items",
                    `List
                      [
                        `Assoc
                          [
                            ("value", `String "spam");
                            ("targets", `List [ `String "content" ]);
                          ];
                      ] );
                ];
              `Assoc
                [
                  ("$type", `String "app.bsky.actor.defs#bskyAppStatePref");
                  ("isBetaUser", `Bool true);
                  ( "nuxs",
                    `List
                      [
                        `Assoc
                          [
                            ("id", `String "welcome"); ("completed", `Bool true);
                          ];
                      ] );
                ];
              `Assoc
                [
                  ("$type", `String "app.bsky.actor.defs#labelersPref");
                  ( "labelers",
                    `List
                      [
                        `Assoc
                          [
                            ("did", `String "did:plc:ar7c4by46qjdydhdevvrndac");
                          ];
                      ] );
                ];
              `Assoc
                [
                  ( "$type",
                    `String "app.bsky.actor.defs#postInteractionSettingsPref" );
                  ( "threadgateAllowRules",
                    `List
                      [
                        `Assoc
                          [
                            ( "$type",
                              `String "app.bsky.feed.threadgate#mentionRule" );
                          ];
                      ] );
                  ( "postgateEmbeddingRules",
                    `List
                      [
                        `Assoc
                          [
                            ( "$type",
                              `String "app.bsky.feed.postgate#disableRule" );
                          ];
                      ] );
                ];
              `Assoc
                [
                  ("$type", `String "app.bsky.actor.defs#verificationPrefs");
                  ("hideBadges", `Bool true);
                ];
              `Assoc
                [
                  ("$type", `String "app.bsky.actor.defs#liveEventPreferences");
                  ("hiddenFeedIds", `List [ `String "live-1" ]);
                  ("hideAllFeeds", `Bool false);
                ];
            ] );
      ]
  in
  let prefs = Actor.parse_preferences json in
  OUnit2.assert_equal 13 (List.length prefs.preferences);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "app.bsky.actor.defs#adultContentPref" (List.hd prefs.preferences).type_;
  (match (List.hd prefs.preferences).kind with
  | `Adult_content a -> OUnit2.assert_equal false a.enabled
  | _ -> OUnit2.assert_failure "expected adultContentPref");
  (match (List.nth prefs.preferences 1).kind with
  | `Saved_feeds_v2 s -> OUnit2.assert_equal 0 (List.length s.items)
  | _ -> OUnit2.assert_failure "expected savedFeedsPrefV2");
  (match (List.nth prefs.preferences 2).kind with
  | `Saved_feeds s -> OUnit2.assert_equal (Some 0) s.timeline_index
  | _ -> OUnit2.assert_failure "expected savedFeedsPref");
  (match (List.nth prefs.preferences 3).kind with
  | `Personal_details p ->
      OUnit2.assert_equal (Some "2000-01-01T00:00:00.000Z") p.birth_date
  | _ -> OUnit2.assert_failure "expected personalDetailsPref");
  (match (List.nth prefs.preferences 4).kind with
  | `Declared_age d -> OUnit2.assert_equal (Some true) d.is_over_age_18
  | _ -> OUnit2.assert_failure "expected declaredAgePref");
  (match (List.nth prefs.preferences 5).kind with
  | `Feed_view f -> OUnit2.assert_equal (Some true) f.hide_replies
  | _ -> OUnit2.assert_failure "expected feedViewPref");
  (match (List.nth prefs.preferences 6).kind with
  | `Thread_view t -> OUnit2.assert_equal (Some "hotness") t.sort
  | _ -> OUnit2.assert_failure "expected threadViewPref");
  (match (List.nth prefs.preferences 7).kind with
  | `Muted_words m ->
      OUnit2.assert_equal ~printer:(fun x -> x) "spam" (List.hd m.items).value
  | _ -> OUnit2.assert_failure "expected mutedWordsPref");
  (match (List.nth prefs.preferences 8).kind with
  | `Bsky_app_state s ->
      OUnit2.assert_equal (Some true) s.is_beta_user;
      OUnit2.assert_equal 1 (List.length s.nuxs)
  | _ -> OUnit2.assert_failure "expected bskyAppStatePref");
  (match (List.nth prefs.preferences 9).kind with
  | `Labelers l -> OUnit2.assert_equal 1 (List.length l.labelers)
  | _ -> OUnit2.assert_failure "expected labelersPref");
  (match (List.nth prefs.preferences 10).kind with
  | `Post_interaction p -> (
      match p.threadgate_allow_rules with
      | Some (`Mention :: _) -> ()
      | _ -> OUnit2.assert_failure "expected mention rule")
  | _ -> OUnit2.assert_failure "expected postInteractionSettingsPref");
  (match (List.nth prefs.preferences 11).kind with
  | `Verification v -> OUnit2.assert_equal true v.hide_badges
  | _ -> OUnit2.assert_failure "expected verificationPrefs");
  match (List.nth prefs.preferences 12).kind with
  | `Live_event e -> OUnit2.assert_equal [ "live-1" ] e.hidden_feed_ids
  | _ -> OUnit2.assert_failure "expected liveEventPreferences"

let test_get_preferences_auth_skipped _ =
  skip_if
    (not Auth.has_live_credentials)
    "ATP_AUTH not configured; live Bluesky test skipped";
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let prefs = Actor.get_preferences test_session in
  OUnit2.assert_bool "preferences parsed" (List.length prefs.preferences >= 0)

let suite =
  "suite"
  >::: [
         "test_get_profile" >:: test_get_profile;
         "test_get_profiles" >:: test_get_profiles;
         "test_get_suggestions" >:: test_get_suggestions;
         "test_search_actors" >:: test_search_actors;
         "test_search_actors_typeahead" >:: test_search_actors_typeahead;
         "test_parse_preferences" >:: test_parse_preferences;
         "test_get_preferences_auth_skipped"
         >:: test_get_preferences_auth_skipped;
       ]

let () = run_test_tt_main suite
