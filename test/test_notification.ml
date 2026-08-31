open OUnit2
open Atproto.Session
open Atproto.Auth
open Atproto.Notification

let create_test_session _ =
  let username, password = Auth.username_and_password_from_env in
  Session.create_session username password

let test_parse_unread_and_like _ =
  let count = Notification.parse_unread_count (`Assoc [ ("count", `Int 3) ]) in
  OUnit2.assert_equal 3 count.count;
  let like =
    Notification.parse_record
      (`Assoc
        [
          ("$type", `String "app.bsky.feed.like");
          ( "subject",
            `Assoc
              [
                ( "uri",
                  `String "at://did:plc:alice/app.bsky.feed.post/3jzfcijpj2z2a"
                );
                ("cid", `String "bafyreihdummy000000000000000000000000000000000");
              ] );
          ("createdAt", `String "2024-01-01T00:00:00.000Z");
        ])
      "like"
  in
  (match like with
  | `Like r ->
      OUnit2.assert_equal
        ~printer:(fun x -> x)
        "app.bsky.feed.like" r.record_type
  | _ -> OUnit2.assert_failure "expected like");
  match
    Notification.parse_record
      (`Assoc
        [
          ("$type", `String "app.bsky.feed.post");
          ("text", `String "hi");
          ("createdAt", `String "2024-01-01T00:00:00.000Z");
        ])
      "quote"
  with
  | `Quote q -> OUnit2.assert_equal ~printer:(fun x -> x) "hi" q.text
  | _ -> OUnit2.assert_failure "expected quote record"

let test_parse_mention_and_via_repost _ =
  (match
     Notification.parse_record
       (`Assoc
         [
           ("$type", `String "app.bsky.feed.post");
           ("text", `String "@alice hello");
           ("createdAt", `String "2024-01-01T00:00:00.000Z");
         ])
       "mention"
   with
  | `Mention m ->
      OUnit2.assert_equal ~printer:(fun x -> x) "@alice hello" m.text
  | _ -> OUnit2.assert_failure "expected mention");
  (match
     Notification.parse_record
       (`Assoc
         [
           ("$type", `String "app.bsky.feed.like");
           ( "subject",
             `Assoc
               [
                 ( "uri",
                   `String "at://did:plc:alice/app.bsky.feed.post/3jzfcijpj2z2a"
                 );
                 ( "cid",
                   `String "bafyreihdummy000000000000000000000000000000000" );
               ] );
           ("createdAt", `String "2024-01-01T00:00:00.000Z");
         ])
       "like-via-repost"
   with
  | `Like_via_repost _ -> ()
  | _ -> OUnit2.assert_failure "expected like-via-repost");
  match Notification.parse_record (`Assoc []) "unknown-reason" with
  | `Other o ->
      OUnit2.assert_equal ~printer:(fun x -> x) "unknown-reason" o.reason
  | _ -> OUnit2.assert_failure "expected other"

let test_parse_preferences _ =
  let json =
    `Assoc
      [
        ( "preferences",
          `Assoc
            [
              ( "chat",
                `Assoc [ ("include", `String "all"); ("push", `Bool true) ] );
              ( "follow",
                `Assoc
                  [
                    ("include", `String "all");
                    ("list", `Bool true);
                    ("push", `Bool false);
                  ] );
              ( "like",
                `Assoc
                  [
                    ("include", `String "follows");
                    ("list", `Bool true);
                    ("push", `Bool true);
                  ] );
              ( "likeViaRepost",
                `Assoc
                  [
                    ("include", `String "all");
                    ("list", `Bool false);
                    ("push", `Bool false);
                  ] );
              ( "mention",
                `Assoc
                  [
                    ("include", `String "all");
                    ("list", `Bool true);
                    ("push", `Bool true);
                  ] );
              ( "quote",
                `Assoc
                  [
                    ("include", `String "all");
                    ("list", `Bool true);
                    ("push", `Bool true);
                  ] );
              ( "reply",
                `Assoc
                  [
                    ("include", `String "all");
                    ("list", `Bool true);
                    ("push", `Bool true);
                  ] );
              ( "repost",
                `Assoc
                  [
                    ("include", `String "all");
                    ("list", `Bool true);
                    ("push", `Bool false);
                  ] );
              ( "repostViaRepost",
                `Assoc
                  [
                    ("include", `String "all");
                    ("list", `Bool false);
                    ("push", `Bool false);
                  ] );
              ( "starterpackJoined",
                `Assoc [ ("list", `Bool true); ("push", `Bool false) ] );
              ( "subscribedPost",
                `Assoc [ ("list", `Bool true); ("push", `Bool true) ] );
              ( "unverified",
                `Assoc [ ("list", `Bool true); ("push", `Bool false) ] );
              ("verified", `Assoc [ ("list", `Bool true); ("push", `Bool true) ]);
            ] );
      ]
  in
  let prefs = Notification.parse_preferences json in
  OUnit2.assert_equal ~printer:(fun x -> x) "follows" prefs.like.include_;
  OUnit2.assert_equal true prefs.verified.push;
  let encoded = Notification.preferences_to_json prefs in
  let open Yojson.Safe.Util in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "follows"
    (encoded |> member "like" |> member "include" |> to_string)

let test_get_unread_count _ =
  skip_if
    (not Auth.has_live_credentials)
    "ATP_AUTH not configured; live Bluesky test skipped";
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let unread_count = Notification.get_unread_count test_session in
  match unread_count with
  | { count } -> OUnit2.assert_bool "Count is not present" (count >= 0)

let test_list_notifications _ =
  skip_if
    (not Auth.has_live_credentials)
    "ATP_AUTH not configured; live Bluesky test skipped";
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let notifications = Notification.list_notifications test_session 10 in
  match notifications with
  | [] -> OUnit2.assert_equal "blah" ""
  | hd :: _ -> (
      match hd with
      | { author; _ } -> (
          match author with
          | { handle; _ } ->
              OUnit2.assert_bool "Handle is empty" (String.length handle > 0)))

let test_update_seen _ =
  skip_if
    (not Auth.has_live_credentials)
    "ATP_AUTH not configured; live Bluesky test skipped";
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let updated_seen =
    Notification.update_seen test_session "2023-07-15T12:34:56.789012Z"
  in
  Printf.printf "Updated Seen: %s\n" updated_seen;
  OUnit2.assert_bool "Updated Seen is not empty" (updated_seen = "")

let suite =
  "suite"
  >::: [
         "test_parse_unread_and_like" >:: test_parse_unread_and_like;
         "test_parse_mention_and_via_repost"
         >:: test_parse_mention_and_via_repost;
         "test_parse_preferences" >:: test_parse_preferences;
         "test_get_unread_count" >:: test_get_unread_count;
         "test_list_notifications" >:: test_list_notifications;
         "test_update_seen" >:: test_update_seen;
       ]

let () = run_test_tt_main suite
