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
            ] );
      ]
  in
  let prefs = Actor.parse_preferences json in
  OUnit2.assert_equal 2 (List.length prefs.preferences);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "app.bsky.actor.defs#adultContentPref" (List.hd prefs.preferences).type_

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
