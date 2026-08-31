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

let test_parse_profile_and_scoped_mute_viewer _ =
  let json =
    `Assoc
      [
        ("did", `String "did:plc:abc123xyz0001112223333");
        ("handle", `String "alice.test");
        ("displayName", `String "Alice");
        ("pronouns", `String "she/her");
        ("website", `String "https://alice.test");
        ("createdAt", `String "2024-01-01T00:00:00.000Z");
        ( "pinnedPost",
          `Assoc
            [
              ( "uri",
                `String
                  "at://did:plc:abc123xyz0001112223333/app.bsky.feed.post/3k" );
              ("cid", `String "bafyreiabc");
            ] );
        ( "joinedViaStarterPack",
          `Assoc
            [
              ( "uri",
                `String
                  "at://did:plc:abc123xyz0001112223333/app.bsky.graph.starterpack/3k"
              );
              ("cid", `String "bafyreipack");
            ] );
        ( "associated",
          `Assoc
            [
              ("lists", `Int 2);
              ("labeler", `Bool true);
              ( "chat",
                `Assoc
                  [
                    ("allowIncoming", `String "following");
                    ("allowGroupInvites", `String "none");
                  ] );
              ( "germ",
                `Assoc
                  [
                    ("showButtonTo", `String "everyone");
                    ("messageMeUrl", `String "https://germ.example/alice");
                  ] );
              ( "activitySubscription",
                `Assoc [ ("allowSubscriptions", `String "followers") ] );
            ] );
        ( "verification",
          `Assoc
            [
              ("verifiedStatus", `String "valid");
              ("trustedVerifierStatus", `String "none");
              ( "verifications",
                `List
                  [
                    `Assoc
                      [
                        ("issuer", `String "did:plc:verifier000111222333444");
                        ( "uri",
                          `String
                            "at://did:plc:verifier/app.bsky.graph.verification/1"
                        );
                        ("isValid", `Bool true);
                        ("createdAt", `String "2024-02-01T00:00:00.000Z");
                      ];
                  ] );
            ] );
        ( "status",
          `Assoc
            [
              ("status", `String "app.bsky.actor.status#live");
              ("isActive", `Bool true);
            ] );
        ( "viewer",
          `Assoc
            [
              ("muted", `Bool false);
              ("mutedOnlyReposts", `Bool true);
              ("mutedOnlyQuoteposts", `Bool false);
              ("blockedBy", `Bool false);
              ("blocking", `String "at://did:plc:me/app.bsky.graph.block/1");
              ( "knownFollowers",
                `Assoc
                  [
                    ("count", `Int 3);
                    ( "followers",
                      `List
                        [
                          `Assoc
                            [
                              ("did", `String "did:plc:abc123xyz0001112223333");
                              ("handle", `String "bob.test");
                            ];
                        ] );
                  ] );
              ( "activitySubscription",
                `Assoc [ ("post", `Bool true); ("reply", `Bool false) ] );
            ] );
      ]
  in
  let profile = Actor.parse_profile json in
  OUnit2.assert_equal (Some "she/her") profile.pronouns;
  OUnit2.assert_equal (Some "https://alice.test") profile.website;
  OUnit2.assert_equal
    (Some "at://did:plc:abc123xyz0001112223333/app.bsky.feed.post/3k")
    profile.pinned_post_uri;
  OUnit2.assert_equal
    (Some "at://did:plc:abc123xyz0001112223333/app.bsky.graph.starterpack/3k")
    profile.joined_via_starter_pack_uri;
  (match profile.associated with
  | Some assoc -> (
      OUnit2.assert_equal (Some 2) assoc.lists;
      OUnit2.assert_equal (Some true) assoc.labeler;
      (match assoc.germ with
      | Some g ->
          OUnit2.assert_equal ~printer:(fun x -> x) "everyone" g.show_button_to
      | None -> OUnit2.assert_failure "expected associated.germ");
      match assoc.chat with
      | Some c ->
          OUnit2.assert_equal ~printer:(fun x -> x) "following" c.allow_incoming
      | None -> OUnit2.assert_failure "expected associated.chat")
  | None -> OUnit2.assert_failure "expected associated");
  (match profile.verification with
  | Some v ->
      OUnit2.assert_equal ~printer:(fun x -> x) "valid" v.verified_status;
      OUnit2.assert_equal 1 (List.length v.verifications)
  | None -> OUnit2.assert_failure "expected verification");
  (match profile.status with
  | Some s -> OUnit2.assert_equal (Some true) s.is_active
  | None -> OUnit2.assert_failure "expected status");
  OUnit2.assert_equal false profile.viewer.muted;
  OUnit2.assert_equal (Some true) profile.viewer.muted_only_reposts;
  OUnit2.assert_equal (Some false) profile.viewer.muted_only_quoteposts;
  (match profile.viewer.known_followers with
  | Some kf ->
      OUnit2.assert_equal 3 kf.count;
      OUnit2.assert_equal
        ~printer:(fun x -> x)
        "bob.test" (List.hd kf.followers).handle
  | None -> OUnit2.assert_failure "expected knownFollowers");
  match profile.viewer.activity_subscription with
  | Some a ->
      OUnit2.assert_equal true a.post;
      OUnit2.assert_equal false a.reply
  | None -> OUnit2.assert_failure "expected viewer activitySubscription"

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
         "test_parse_profile_and_scoped_mute_viewer"
         >:: test_parse_profile_and_scoped_mute_viewer;
         "test_get_preferences_auth_skipped"
         >:: test_get_preferences_auth_skipped;
       ]

let () = run_test_tt_main suite
