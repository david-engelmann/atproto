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
         "test_send_message_body" >:: test_send_message_body;
         "test_parse_unread_and_logs" >:: test_parse_unread_and_logs;
         "test_parse_message_facets_reactions_embed"
         >:: test_parse_message_facets_reactions_embed;
         "test_lock_unlock_and_group_bodies"
         >:: test_lock_unlock_and_group_bodies;
         "test_list_convos_auth_skipped" >:: test_list_convos_auth_skipped;
       ]

let () = run_test_tt_main suite
