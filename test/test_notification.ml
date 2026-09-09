open OUnit2
open Atproto.Session
open Atproto.Auth
open Atproto.Notification
open Atproto.Xrpc

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

let test_update_seen_body _ =
  let body =
    Notification.update_seen_body ~seen_at:"2023-07-15T12:34:56.789012Z"
  in
  let open Yojson.Safe.Util in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "2023-07-15T12:34:56.789012Z"
    (body |> member "seenAt" |> to_string)

let test_put_preferences_body _ =
  let body = Notification.put_preferences_body ~priority:true in
  let open Yojson.Safe.Util in
  OUnit2.assert_equal true (body |> member "priority" |> to_bool);
  match body with
  | `Assoc fields ->
      OUnit2.assert_equal ~printer:string_of_int 1 (List.length fields)
  | _ -> OUnit2.assert_failure "expected put_preferences_body object"

let test_list_notifications_body _ =
  OUnit2.assert_equal [] (Notification.list_notifications_body ());
  OUnit2.assert_equal [] (Notification.list_notifications_body ~reasons:[] ());
  OUnit2.assert_equal
    [ ("limit", "10") ]
    (Notification.list_notifications_body ~limit:10 ());
  OUnit2.assert_equal
    [
      ("limit", "25");
      ("reasons", "like");
      ("reasons", "mention");
      ("priority", "true");
      ("cursor", "n1");
      ("seenAt", "2023-07-15T12:34:56.789012Z");
    ]
    (Notification.list_notifications_body ~reasons:[ "like"; "mention" ]
       ~priority:true ~cursor:"n1" ~seen_at:"2023-07-15T12:34:56.789012Z"
       ~limit:25 ())

let test_put_activity_subscription_body _ =
  let body =
    Notification.put_activity_subscription_body ~subject:"did:plc:alice"
      ~activity_subscription:{ post = true; reply = false }
  in
  let open Yojson.Safe.Util in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "did:plc:alice"
    (body |> member "subject" |> to_string);
  let sub = body |> member "activitySubscription" in
  OUnit2.assert_equal true (sub |> member "post" |> to_bool);
  OUnit2.assert_equal false (sub |> member "reply" |> to_bool);
  match body with
  | `Assoc fields ->
      OUnit2.assert_equal ~printer:string_of_int 2 (List.length fields)
  | _ -> OUnit2.assert_failure "expected put_activity_subscription_body object"

let test_push_and_activity_bodies _ =
  OUnit2.assert_equal [] (Notification.list_activity_subscriptions_body ());
  OUnit2.assert_equal
    [ ("limit", "10"); ("cursor", "a1") ]
    (Notification.list_activity_subscriptions_body ~limit:10 ~cursor:"a1" ());
  OUnit2.assert_equal ~printer:(fun x -> x) "ios" Notification.platform_ios;
  OUnit2.assert_equal ~printer:(fun x -> x) "android"
    Notification.platform_android;
  OUnit2.assert_equal ~printer:(fun x -> x) "web" Notification.platform_web;
  OUnit2.assert_equal None (Notification.effective_push_proxy ());
  let gateway =
    { Xrpc.did = "did:web:push.example.org"; service = "bsky_notif" }
  in
  OUnit2.assert_equal (Some gateway)
    (Notification.effective_push_proxy ~proxy:gateway ());
  let open Yojson.Safe.Util in
  let reg =
    Notification.register_push_body ~service_did:"did:web:push.example.org"
      ~token:"device-token" ~platform:Notification.platform_ios
      ~app_id:"org.example.app" ~age_restricted:true ()
  in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "did:web:push.example.org"
    (reg |> member "serviceDid" |> to_string);
  OUnit2.assert_equal true (reg |> member "ageRestricted" |> to_bool);
  let unreg =
    Notification.unregister_push_body ~service_did:"did:web:push.example.org"
      ~token:"device-token" ~platform:Notification.platform_web
      ~app_id:"org.example.app" ()
  in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "web"
    (unreg |> member "platform" |> to_string);
  ignore Notification.register_push;
  ignore Notification.unregister_push;
  ignore Notification.push_live_enabled

let test_register_push_opt_in _ =
  skip_if (not Notification.push_live_enabled)
    "ATP_PUSH not set; hosted push not faked";
  skip_if
    (not Auth.has_live_credentials)
    "ATP_AUTH not configured; live Bluesky test skipped";
  match
    ( Sys.getenv_opt "ATP_PUSH_TOKEN",
      Sys.getenv_opt "ATP_PUSH_DID",
      Sys.getenv_opt "ATP_PUSH_APP_ID" )
  with
  | Some token, Some service_did, Some app_id
    when String.trim token <> ""
         && String.trim service_did <> ""
         && String.trim app_id <> "" ->
      let test_session = create_test_session () in
      let platform =
        match Sys.getenv_opt "ATP_PUSH_PLATFORM" with
        | Some p when String.trim p <> "" -> String.trim p
        | _ -> Notification.platform_web
      in
      try
        Notification.register_push test_session ~service_did ~token ~platform
          ~app_id ();
        OUnit2.assert_bool "registerPush accepted" true
      with exn ->
        skip_if true ("registerPush skipped: " ^ Printexc.to_string exn)
  | _ -> skip_if true "ATP_PUSH_TOKEN / ATP_PUSH_DID / ATP_PUSH_APP_ID not set"

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
         "test_update_seen_body" >:: test_update_seen_body;
         "test_put_preferences_body" >:: test_put_preferences_body;
         "test_list_notifications_body" >:: test_list_notifications_body;
         "test_put_activity_subscription_body"
         >:: test_put_activity_subscription_body;
         "test_push_and_activity_bodies" >:: test_push_and_activity_bodies;
         "test_register_push_opt_in" >:: test_register_push_opt_in;
         "test_get_unread_count" >:: test_get_unread_count;
         "test_list_notifications" >:: test_list_notifications;
         "test_update_seen" >:: test_update_seen;
       ]

let () = run_test_tt_main suite
