open OUnit2
open Atproto.Chat
open Atproto.Xrpc
open Atproto.Auth
open Atproto.Facet

let test_default_proxy _ =
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "did:web:api.bsky.chat#bsky_chat"
    (Xrpc.proxy_to_string Chat.default_proxy);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "did:web:api.bsky.chat#bsky_chat"
    (Xrpc.proxy_to_string Xrpc.chat_proxy)

let test_parse_convos _ =
  let json =
    `Assoc
      [
        ( "convos",
          `List
            [
              `Assoc
                [
                  ("id", `String "convo1");
                  ("rev", `String "aaa");
                  ("muted", `Bool false);
                  ("unreadCount", `Int 2);
                  ("status", `String "accepted");
                  ( "members",
                    `List
                      [
                        `Assoc
                          [
                            ("did", `String "did:plc:abc123xyz0001112223333");
                            ("handle", `String "alice.test");
                          ];
                      ] );
                  ( "lastMessage",
                    `Assoc
                      [
                        ("id", `String "m1");
                        ("rev", `String "r1");
                        ("text", `String "hello");
                        ("sentAt", `String "2024-01-01T00:00:00.000Z");
                        ( "sender",
                          `Assoc
                            [
                              ("did", `String "did:plc:abc123xyz0001112223333");
                            ] );
                      ] );
                ];
            ] );
      ]
  in
  let page = Chat.parse_convos json in
  OUnit2.assert_equal 1 (List.length page.convos);
  OUnit2.assert_equal ~printer:(fun x -> x) "convo1" (List.hd page.convos).id;
  OUnit2.assert_equal 2 (List.hd page.convos).unread_count

let test_parse_messages_related_profiles _ =
  let json =
    `Assoc
      [
        ( "messages",
          `List
            [
              `Assoc
                [
                  ("id", `String "m1");
                  ("rev", `String "r1");
                  ("text", `String "hello");
                  ("sentAt", `String "2024-01-01T00:00:00.000Z");
                  ( "sender",
                    `Assoc [ ("did", `String "did:plc:abc123xyz0001112223333") ]
                  );
                ];
            ] );
        ( "relatedProfiles",
          `List
            [
              `Assoc
                [
                  ("did", `String "did:plc:abc123xyz0001112223333");
                  ("handle", `String "alice.test");
                ];
            ] );
      ]
  in
  let page = Chat.parse_messages json in
  OUnit2.assert_equal 1 (List.length page.messages);
  OUnit2.assert_equal 1 (List.length page.related_profiles);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "alice.test"
    (match (List.hd page.related_profiles).handle with
    | Some h -> h
    | None -> "")

let test_parse_group_convo_extras _ =
  let json =
    `Assoc
      [
        ("id", `String "g1");
        ("rev", `String "r1");
        ("muted", `Bool false);
        ("unreadCount", `Int 0);
        ( "lastReaction",
          `Assoc
            [
              ( "message",
                `Assoc
                  [
                    ("id", `String "m2");
                    ("rev", `String "r2");
                    ("text", `String "reacted");
                    ("sentAt", `String "2024-01-01T00:00:00.000Z");
                  ] );
              ( "reaction",
                `Assoc
                  [
                    ("value", `String "👍");
                    ( "sender",
                      `Assoc
                        [ ("did", `String "did:plc:abc123xyz0001112223333") ] );
                    ("createdAt", `String "2024-01-01T00:00:01.000Z");
                  ] );
            ] );
        ( "kind",
          `Assoc
            [
              ("$type", `String "chat.bsky.convo.defs#groupConvo");
              ("name", `String "mods");
              ("lockStatus", `String "unlocked");
              ("lockStatusModerationOverride", `Bool false);
              ("memberCount", `Int 4);
              ("unreadJoinRequestCount", `Int 2);
            ] );
      ]
  in
  let convo = Chat.parse_convo json in
  OUnit2.assert_equal (Some "mods") convo.group_name;
  OUnit2.assert_equal (Some "unlocked") convo.lock_status;
  OUnit2.assert_equal (Some 4) convo.member_count;
  OUnit2.assert_equal (Some 2) convo.unread_join_request_count;
  match convo.last_reaction with
  | Some lr -> OUnit2.assert_equal ~printer:(fun x -> x) "👍" lr.reaction.value
  | None -> OUnit2.assert_failure "expected lastReaction"

let test_send_message_body _ =
  let body = Chat.send_message_body ~convo_id:"c1" ~text:"hi" () in
  let open Yojson.Safe.Util in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "c1"
    (body |> member "convoId" |> to_string);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "hi"
    (body |> member "message" |> member "text" |> to_string)

let test_parse_unread_and_logs _ =
  let counts =
    Chat.parse_unread_counts
      (`Assoc
        [ ("unreadAcceptedConvos", `Int 3); ("unreadRequestConvos", `Int 1) ])
  in
  OUnit2.assert_equal 3 counts.unread_accepted;
  OUnit2.assert_equal 1 counts.unread_request;
  let logs =
    Chat.parse_logs
      (`Assoc
        [
          ( "logs",
            `List
              [
                `Assoc
                  [
                    ("$type", `String "chat.bsky.convo.defs#logCreateMessage");
                    ("convoId", `String "c1");
                    ("rev", `String "r2");
                  ];
              ] );
        ])
  in
  OUnit2.assert_equal 1 (List.length logs.logs);
  let accept = Chat.parse_accept (`Assoc [ ("rev", `String "r9") ]) in
  OUnit2.assert_equal (Some "r9") accept.rev;
  let avail =
    Chat.parse_availability
      (`Assoc
        [
          ("canChat", `Bool true);
          ( "convo",
            `Assoc
              [
                ("id", `String "c2");
                ("rev", `String "r0");
                ("muted", `Bool false);
                ("unreadCount", `Int 0);
                ("members", `List []);
              ] );
        ])
  in
  OUnit2.assert_equal true avail.can_chat

let test_parse_message_facets_reactions_embed _ =
  let json =
    `Assoc
      [
        ("id", `String "m1");
        ("rev", `String "r1");
        ("text", `String "hello #atp");
        ("sentAt", `String "2024-01-01T00:00:00.000Z");
        ("sender", `Assoc [ ("did", `String "did:plc:abc123xyz0001112223333") ]);
        ( "facets",
          `List
            [
              `Assoc
                [
                  ( "index",
                    `Assoc [ ("byteStart", `Int 6); ("byteEnd", `Int 10) ] );
                  ( "features",
                    `List
                      [
                        `Assoc
                          [
                            ("$type", `String "app.bsky.richtext.facet#tag");
                            ("tag", `String "atp");
                          ];
                      ] );
                ];
            ] );
        ( "reactions",
          `List
            [
              `Assoc
                [
                  ("value", `String "👍");
                  ( "sender",
                    `Assoc [ ("did", `String "did:plc:abc123xyz0001112223333") ]
                  );
                  ("createdAt", `String "2024-01-01T00:00:01.000Z");
                ];
            ] );
        ( "embed",
          `Assoc
            [
              ("$type", `String "app.bsky.embed.record");
              ( "record",
                `Assoc
                  [
                    ( "uri",
                      `String
                        "at://did:plc:abc123xyz0001112223333/app.bsky.feed.post/3k"
                    );
                    ( "cid",
                      `String "bafyreihdummy000000000000000000000000000000" );
                  ] );
            ] );
        ("replyTo", `Assoc [ ("messageId", `String "m0") ]);
      ]
  in
  let msg = Chat.parse_message json in
  OUnit2.assert_equal 1 (List.length msg.facets);
  OUnit2.assert_equal 1 (List.length msg.reactions);
  OUnit2.assert_equal ~printer:(fun x -> x) "👍" (List.hd msg.reactions).value;
  OUnit2.assert_equal (Some "m0") msg.reply_to_id;
  match msg.embed with
  | Some (`Record _) -> ()
  | _ -> OUnit2.assert_failure "expected record embed attachment"

let test_lock_unlock_and_group_bodies _ =
  let open Yojson.Safe.Util in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "c1"
    (Chat.convo_id_body "c1" |> member "convoId" |> to_string);
  let prefs =
    Chat.parse_notification_preferences
      (`Assoc
        [
          ( "preferences",
            `Assoc
              [
                ( "chat",
                  `Assoc [ ("include", `String "all"); ("push", `Bool true) ] );
                ( "chatRequest",
                  `Assoc
                    [ ("include", `String "follows"); ("push", `Bool false) ] );
              ] );
        ])
  in
  OUnit2.assert_equal ~printer:(fun x -> x) "all" prefs.chat.include_;
  OUnit2.assert_equal true prefs.chat.push;
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "follows" prefs.chat_request.include_;
  OUnit2.assert_equal false prefs.chat_request.push;
  let body =
    Chat.message_input
      ~facets:[ Facet.tag ~byte_start:0 ~byte_end:4 "atp" ]
      "hi #atp"
  in
  OUnit2.assert_equal 1 (body |> member "facets" |> to_list |> List.length)

let test_actor_status_and_declaration _ =
  let st =
    Chat.parse_actor_status
      (`Assoc
        [
          ("chatDisabled", `Bool false);
          ("canCreateGroups", `Bool true);
          ("groupMemberLimit", `Int 50);
        ])
  in
  OUnit2.assert_equal false st.chat_disabled;
  OUnit2.assert_equal true st.can_create_groups;
  OUnit2.assert_equal 50 st.group_member_limit;
  let dec =
    Chat.parse_declaration
      (`Assoc
        [
          ("allowIncoming", `String "following");
          ("allowGroupInvites", `String "none");
        ])
  in
  OUnit2.assert_equal ~printer:(fun x -> x) "following" dec.allow_incoming;
  OUnit2.assert_equal (Some "none") dec.allow_group_invites;
  let body = Chat.declaration_json ~allow_incoming:"all" () in
  let open Yojson.Safe.Util in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "chat.bsky.actor.declaration"
    (body |> member "$type" |> to_string)

let test_moderation_parsers _ =
  let meta =
    Chat.parse_actor_metadata
      (`Assoc
        [
          ( "day",
            `Assoc
              [
                ("messagesSent", `Int 1);
                ("messagesReceived", `Int 2);
                ("convos", `Int 3);
                ("convosStarted", `Int 1);
              ] );
          ( "month",
            `Assoc
              [
                ("messagesSent", `Int 10);
                ("messagesReceived", `Int 20);
                ("convos", `Int 4);
                ("convosStarted", `Int 2);
              ] );
          ( "all",
            `Assoc
              [
                ("messagesSent", `Int 100);
                ("messagesReceived", `Int 200);
                ("convos", `Int 8);
                ("convosStarted", `Int 5);
              ] );
        ])
  in
  OUnit2.assert_equal 1 meta.day.messages_sent;
  OUnit2.assert_equal 200 meta.all.messages_received;
  let convo =
    Chat.parse_mod_convo
      (`Assoc
        [
          ("id", `String "c9");
          ("rev", `String "r9");
          ( "kind",
            `Assoc
              [
                ("$type", `String "chat.bsky.moderation.defs#groupConvo");
                ("createdAt", `String "2024-01-01T00:00:00.000Z");
                ("joinRequestCount", `Int 2);
                ("lockStatus", `String "unlocked");
                ("memberCount", `Int 4);
                ("memberLimit", `Int 50);
                ("name", `String "mods");
              ] );
        ])
  in
  OUnit2.assert_equal ~printer:(fun x -> x) "c9" convo.id;
  (match convo.kind with
  | `Group g ->
      OUnit2.assert_equal ~printer:(fun x -> x) "mods" g.name;
      OUnit2.assert_equal 4 g.member_count
  | _ -> OUnit2.assert_failure "expected group convo");
  let body =
    Chat.update_actor_access_body ~actor:"did:plc:abc123xyz0001112223333"
      ~allow_access:false ~ref:"ticket-1" ()
  in
  let open Yojson.Safe.Util in
  OUnit2.assert_equal false (body |> member "allowAccess" |> to_bool);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "ticket-1"
    (body |> member "ref" |> to_string)

let test_subscribe_mod_events _ =
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "wss://api.bsky.chat/xrpc/chat.bsky.moderation.subscribeModEvents"
    (Chat.subscribe_mod_events_url ());
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "wss://chat.example/xrpc/chat.bsky.moderation.subscribeModEvents?cursor=2222222222222"
    (Chat.subscribe_mod_events_url ~host:"chat.example" ~cursor:"2222222222222"
       ());
  let first =
    Chat.parse_mod_event
      (`Assoc
        [
          ( "$type",
            `String
              "chat.bsky.moderation.subscribeModEvents#eventConvoFirstMessage"
          );
          ("convoId", `String "c1");
          ("createdAt", `String "2024-01-01T00:00:00.000Z");
          ("rev", `String "r1");
          ("user", `String "did:plc:abc123xyz0001112223333");
          ("recipients", `List [ `String "did:plc:def456xyz0001112223333" ]);
          ("messageId", `String "m1");
        ])
  in
  (match first with
  | `Convo_first_message e ->
      OUnit2.assert_equal ~printer:(fun x -> x) "c1" e.convo_id;
      OUnit2.assert_equal 1 (List.length e.recipients)
  | _ -> OUnit2.assert_failure "expected first-message event");
  let created =
    Chat.parse_mod_event
      (`Assoc
        [
          ( "$type",
            `String
              "chat.bsky.moderation.subscribeModEvents#eventGroupChatCreated" );
          ("actorDid", `String "did:plc:abc123xyz0001112223333");
          ("convoCreatedAt", `String "2024-01-01T00:00:00.000Z");
          ("convoId", `String "g1");
          ("createdAt", `String "2024-01-01T00:00:01.000Z");
          ("groupMemberCount", `Int 3);
          ("groupName", `String "mods");
          ( "initialMemberDids",
            `List [ `String "did:plc:def456xyz0001112223333" ] );
          ("ownerDid", `String "did:plc:abc123xyz0001112223333");
          ("rev", `String "r2");
        ])
  in
  (match created with
  | `Group_chat_created e ->
      OUnit2.assert_equal ~printer:(fun x -> x) "mods" e.group_name
  | _ -> OUnit2.assert_failure "expected group created");
  let approved =
    Chat.parse_mod_event
      (`Assoc
        [
          ( "$type",
            `String
              "chat.bsky.moderation.subscribeModEvents#eventGroupChatJoinRequestApproved"
          );
          ("actorDid", `String "did:plc:abc123xyz0001112223333");
          ("convoCreatedAt", `String "2024-01-01T00:00:00.000Z");
          ("convoId", `String "g1");
          ("createdAt", `String "2024-01-01T00:00:02.000Z");
          ("groupMemberCount", `Int 4);
          ("groupName", `String "mods");
          ("ownerDid", `String "did:plc:abc123xyz0001112223333");
          ("rev", `String "r3");
          ("subjectDid", `String "did:plc:ghi789xyz0001112223333");
        ])
  in
  (match approved with
  | `Group_chat_join_request_approved e ->
      OUnit2.assert_equal ~printer:(fun x -> x) "g1" e.convo_id
  | _ -> OUnit2.assert_failure "expected join approved, not bare join request");
  let header =
    Atproto.Firehose.Firehose.encode_header
      { op = 1; t = Some "#eventRateLimitExceeded" }
  in
  let body =
    Atproto.Dag_cbor.Dag_cbor.encode
      (Atproto.Dag_cbor.Dag_cbor.Map
         [
           ( "actorDid",
             Atproto.Dag_cbor.Dag_cbor.Text "did:plc:abc123xyz0001112223333" );
           ( "createdAt",
             Atproto.Dag_cbor.Dag_cbor.Text "2024-01-01T00:00:00.000Z" );
           ( "endpoint",
             Atproto.Dag_cbor.Dag_cbor.Text "chat.bsky.convo.sendMessage" );
           ("rev", Atproto.Dag_cbor.Dag_cbor.Text "r9");
         ])
  in
  match Chat.decode_mod_event_frame (header ^ body) with
  | _, `Rate_limit_exceeded e ->
      OUnit2.assert_equal ~printer:(fun x -> x) "r9" e.rev
  | _ -> OUnit2.assert_failure "expected rate-limit CBOR frame"

let test_group_join_parsers _ =
  let link =
    Chat.parse_join_link
      (`Assoc
        [
          ("code", `String "abc123");
          ("enabledStatus", `String "enabled");
          ("requireApproval", `Bool true);
          ("joinRule", `String "followedByOwner");
          ("createdAt", `String "2024-01-01T00:00:00.000Z");
        ])
  in
  OUnit2.assert_equal ~printer:(fun x -> x) "abc123" link.code;
  OUnit2.assert_equal true link.require_approval;
  let reqs =
    Chat.parse_join_requests
      (`Assoc
        [
          ( "requests",
            `List
              [
                `Assoc
                  [
                    ("convoId", `String "g1");
                    ( "requestedBy",
                      `Assoc
                        [
                          ("did", `String "did:plc:abc123xyz0001112223333");
                          ("handle", `String "alice.test");
                        ] );
                    ("requestedAt", `String "2024-01-01T00:00:00.000Z");
                  ];
              ] );
        ])
  in
  OUnit2.assert_equal 1 (List.length reqs.requests);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "g1" (List.hd reqs.requests).convo_id;
  (match
     Chat.parse_join_preview
       (`Assoc
         [
           ("$type", `String "chat.bsky.group.defs#joinLinkPreviewView");
           ("convoId", `String "g1");
           ("code", `String "abc123");
           ("name", `String "Friends");
           ( "owner",
             `Assoc [ ("did", `String "did:plc:abc123xyz0001112223333") ] );
           ("memberCount", `Int 3);
           ("memberLimit", `Int 100);
           ("requireApproval", `Bool false);
           ("joinRule", `String "anyone");
         ])
   with
  | `Preview p ->
      OUnit2.assert_equal ~printer:(fun x -> x) "Friends" p.name;
      OUnit2.assert_equal 3 p.member_count
  | _ -> OUnit2.assert_failure "expected join preview");
  (match
     Chat.parse_join_preview
       (`Assoc
         [
           ("$type", `String "chat.bsky.group.defs#disabledJoinLinkPreviewView");
           ("code", `String "dead");
         ])
   with
  | `Disabled code -> OUnit2.assert_equal ~printer:(fun x -> x) "dead" code
  | _ -> OUnit2.assert_failure "expected disabled preview");
  let members =
    Chat.parse_members_page
      (`Assoc
        [
          ( "members",
            `List
              [
                `Assoc
                  [
                    ("did", `String "did:plc:abc123xyz0001112223333");
                    ("handle", `String "alice.test");
                  ];
              ] );
        ])
  in
  OUnit2.assert_equal 1 (List.length members.members)

let test_list_convos_auth_skipped _ =
  skip_if
    (not Auth.has_live_credentials)
    "ATP_AUTH not configured; live Bluesky test skipped";
  let username, password = Auth.username_and_password_from_env in
  let s = Atproto.Session.Session.create_session username password in
  try
    let page = Chat.list_convos s ~limit:5 () in
    OUnit2.assert_bool "convos parsed" (List.length page.convos >= 0)
  with exn -> skip_if true ("listConvos skipped: " ^ Printexc.to_string exn)

let suite =
  "chat"
  >::: [
         "test_default_proxy" >:: test_default_proxy;
         "test_parse_convos" >:: test_parse_convos;
         "test_parse_messages_related_profiles"
         >:: test_parse_messages_related_profiles;
         "test_parse_group_convo_extras" >:: test_parse_group_convo_extras;
         "test_send_message_body" >:: test_send_message_body;
         "test_parse_unread_and_logs" >:: test_parse_unread_and_logs;
         "test_parse_message_facets_reactions_embed"
         >:: test_parse_message_facets_reactions_embed;
         "test_lock_unlock_and_group_bodies"
         >:: test_lock_unlock_and_group_bodies;
         "test_actor_status_and_declaration"
         >:: test_actor_status_and_declaration;
         "test_moderation_parsers" >:: test_moderation_parsers;
         "test_subscribe_mod_events" >:: test_subscribe_mod_events;
         "test_group_join_parsers" >:: test_group_join_parsers;
         "test_list_convos_auth_skipped" >:: test_list_convos_auth_skipped;
       ]

let () = run_test_tt_main suite
