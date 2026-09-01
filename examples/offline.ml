(* Compiled (not live-run) examples for the public API documented in README.
   `dune build` typechecks this against the current modules. *)

open Atproto.Video
open Atproto.Embed
open Atproto.Facet
open Atproto.Records
open Atproto.Notification
open Atproto.Feed
open Atproto.Chat
open Atproto.Ozone
open Atproto.Identity
open Atproto.Lexicon
open Atproto.Tid
open Atproto.Mst
open Atproto.Oauth
open Atproto.Xrpc
open Atproto.Jetstream
open Atproto.Unspecced
open Atproto.Draft
open Atproto.Contact
open Atproto.Ageassurance
open Atproto.Actor
open Atproto.Oauth_scope
open Atproto.Repo_sync
open Atproto.Cid
open Atproto.Repo
open Atproto.Server
open Atproto.Http_method
open Atproto.Hash
open Atproto.Varint
open Atproto.Syntax
open Atproto.Temp
open Atproto.Graph
open Atproto.Site
open Atproto.Germnetwork
open Atproto.Admin
open Atproto.Request
open Atproto.Response
open Atproto.Http_client
open Atproto.Auth
open Atproto.Session
open Atproto.Label

let () =
  (* TID used as record keys and commit revs *)
  assert (Tid.is_valid "3jzfcijpj2z2a");
  (* MST layer for a repo key — official vector *)
  assert (Mst.layer_for_key "blue" = 1);
  (* OAuth client metadata (no hosted client required) *)
  let meta =
    Oauth.public_metadata
      ~client_id:"https://client.example/client-metadata.json"
      ~redirect_uris:[ "https://client.example/cb" ]
      ()
  in
  Oauth.validate_metadata meta;
  (* Video byte-upload pipeline — URL, service-auth audience, embed JSON *)
  let url =
    Video.upload_video_url ~did:"did:plc:abc123xyz0001112223333"
      ~name:"clip.mp4" ()
  in
  assert (String.length url > 0);
  let exp = Video.recommended_exp ~now:1_700_000_000.0 () in
  assert (exp = Int64.add 1_700_000_000L Video.recommended_exp_seconds);
  let blob =
    `Assoc
      [
        ("$type", `String "blob");
        ("ref", `Assoc [ ("$link", `String "bafyvideo") ]);
        ("mimeType", `String "video/mp4");
        ("size", `Int 8);
      ]
  in
  let embed =
    Video.video_embed_json ~video:blob ~alt:"demo" ~presentation:"gif" ()
  in
  assert (
    match Yojson.Safe.Util.member "$type" embed with
    | `String "app.bsky.embed.video" -> true
    | _ -> false);
  assert (
    match Yojson.Safe.Util.member "presentation" embed with
    | `String "gif" -> true
    | _ -> false);
  let start =
    Video.start_upload_body ~size_bytes:1_048_576 ~mime_type:"video/mp4"
      ~name:"clip.mp4" ()
  in
  assert (
    match Yojson.Safe.Util.member "sizeBytes" start with
    | `Int 1048576 -> true
    | _ -> false);
  let upload_st =
    Video.parse_upload_status
      (`Assoc
        [
          ("jobId", `String "job-m");
          ("partSizeBytes", `Int 100);
          ("partCount", `Int 2);
          ("receivedParts", `List [ `Int 1 ]);
          ("expiresAt", `String "2026-01-01T00:00:00.000Z");
          ("state", `String "created");
        ])
  in
  assert (Video.missing_parts upload_st = [ 2 ]);
  (match
     Embed.parse_embed
       (`Assoc
         [
           ("$type", `String "app.bsky.embed.video");
           ("video", blob);
           ("alt", `String "demo");
         ])
   with
  | `Video _ -> ()
  | _ -> assert false);
  let facet =
    Facet.parse_facet
      (`Assoc
        [
          ("index", `Assoc [ ("byteStart", `Int 0); ("byteEnd", `Int 4) ]);
          ( "features",
            `List
              [
                `Assoc
                  [
                    ("$type", `String "app.bsky.richtext.facet#tag");
                    ("tag", `String "atp");
                  ];
              ] );
        ])
  in
  (match facet with `Tag _ -> () | _ -> assert false);
  let built = Facet.tag ~byte_start:0 ~byte_end:4 "atp" in
  (match Facet.parse_facet (Facet.facet_to_json built) with
  | `Tag t -> assert ((List.hd t.features).tag = "atp")
  | _ -> assert false);
  (match
     Embed.parse_embed
       (`Assoc
         [
           ("$type", `String "app.bsky.embed.gallery");
           ( "items",
             `List
               [
                 `Assoc
                   [
                     ("alt", `String "pic");
                     ( "image",
                       `Assoc
                         [
                           ("$type", `String "blob");
                           ("ref", `Assoc [ ("$link", `String "bafyimage") ]);
                           ("mimeType", `String "image/png");
                           ("size", `Int 4);
                         ] );
                   ];
               ] );
         ])
   with
  | `Gallery g -> assert (List.length g.items = 1)
  | _ -> assert false);
  let post =
    Records.post ~text:"hello" ~created_at:"2024-01-01T00:00:00.000Z"
      ~tags:[ "atp" ] ()
  in
  let status =
    Records.status ~status:Records.status_live
      ~created_at:"2024-01-01T00:00:00.000Z" ()
  in
  assert (
    match Yojson.Safe.Util.member "$type" status with
    | `String "app.bsky.actor.status" -> true
    | _ -> false);
  let liked =
    Feed.parse_post
      (`Assoc
        [
          ( "uri",
            `String "at://did:plc:abc123xyz0001112223333/app.bsky.feed.post/3k"
          );
          ("cid", `String "bafyreiabc");
          ( "author",
            `Assoc
              [
                ("did", `String "did:plc:abc123xyz0001112223333");
                ("handle", `String "alice.test");
              ] );
          ("record", `Assoc [ ("text", `String "hi") ]);
          ("indexedAt", `String "2024-01-01T00:00:00.000Z");
          ( "viewer",
            `Assoc
              [
                ( "knownLikers",
                  `Assoc
                    [
                      ("count", `Int 1);
                      ( "actors",
                        `List
                          [
                            `Assoc
                              [
                                ("did", `String "did:plc:abc123xyz0001112223333");
                                ("handle", `String "alice.test");
                              ];
                          ] );
                    ] );
              ] );
        ])
  in
  (match liked.known_likers with
  | Some kl -> assert (kl.count = 1)
  | None -> assert false);
  assert (liked.bookmarked = None);
  let mute = Graph.mute_actor_body ~actor:"alice.test" ~only_reposts:true () in
  assert (
    match Yojson.Safe.Util.member "onlyReposts" mute with
    | `Bool true -> true
    | _ -> false);
  let reply =
    Feed.parse_reply
      (`Assoc
        [
          ( "root",
            `Assoc
              [
                ( "uri",
                  `String
                    "at://did:plc:abc123xyz0001112223333/app.bsky.feed.post/r"
                );
                ("cid", `String "bafyreiroot");
                ( "author",
                  `Assoc
                    [
                      ("did", `String "did:plc:abc123xyz0001112223333");
                      ("handle", `String "alice.test");
                    ] );
                ("record", `Assoc [ ("text", `String "root") ]);
                ("indexedAt", `String "2024-01-01T00:00:00.000Z");
              ] );
          ( "parent",
            `Assoc
              [
                ( "uri",
                  `String
                    "at://did:plc:abc123xyz0001112223333/app.bsky.feed.post/p"
                );
                ("cid", `String "bafyreiparent");
                ( "author",
                  `Assoc
                    [
                      ("did", `String "did:plc:abc123xyz0001112223333");
                      ("handle", `String "alice.test");
                    ] );
                ("record", `Assoc [ ("text", `String "parent") ]);
                ("indexedAt", `String "2024-01-01T00:00:00.000Z");
              ] );
          ( "grandparentAuthor",
            `Assoc
              [
                ("did", `String "did:plc:abc123xyz0001112223333");
                ("handle", `String "alice.test");
              ] );
        ])
  in
  (match reply.grandparent_author with
  | Some a -> assert (a.handle = "alice.test")
  | None -> assert false);
  let status =
    Server.parse_account_status
      (`Assoc
        [
          ("activated", `Bool true);
          ("validDid", `Bool true);
          ("repoCommit", `String "bafyreicommit");
          ("repoRev", `String "3jzfcijpj2z2a");
          ("repoBlocks", `Int 1);
          ("indexedRecords", `Int 1);
          ("privateStateValues", `Int 0);
          ("expectedBlobs", `Int 0);
          ("importedBlobs", `Int 0);
        ])
  in
  assert (status.repo_rev = Some "3jzfcijpj2z2a");
  let invites =
    Server.create_invite_codes_body ~code_count:1 ~use_count:1
      ~for_accounts:[ "did:plc:abc123xyz0001112223333" ]
      ()
  in
  assert (
    match Yojson.Safe.Util.member "codeCount" invites with
    | `Int 1 -> true
    | _ -> false);
  let lvd =
    Label.parse_label_value_definition
      (`Assoc
        [
          ("identifier", `String "spam");
          ("severity", `String "inform");
          ("blurs", `String "none");
          ("locales", `List []);
        ])
  in
  assert (lvd.identifier = "spam");
  let session_body =
    Auth.create_session_body ~identifier:"alice.test" ~password:"x"
      ~allow_takendown:true ()
  in
  assert (
    match Yojson.Safe.Util.member "allowTakendown" session_body with
    | `Bool true -> true
    | _ -> false);
  let acct =
    Server.create_account_body ~handle:"alice.test" ~verification_code:"123456"
      ()
  in
  assert (
    match Yojson.Safe.Util.member "verificationCode" acct with
    | `String "123456" -> true
    | _ -> false);
  let app_pw =
    Server.create_app_password_body ~name:"cli" ~privileged:true ()
  in
  assert (
    match Yojson.Safe.Util.member "privileged" app_pw with
    | `Bool true -> true
    | _ -> false);
  let profile_view =
    Actor.parse_profile
      (`Assoc
        [
          ("did", `String "did:plc:abc123xyz0001112223333");
          ("handle", `String "alice.test");
          ("pronouns", `String "she/her");
          ( "associated",
            `Assoc
              [
                ( "germ",
                  `Assoc
                    [
                      ("showButtonTo", `String "everyone");
                      ("messageMeUrl", `String "https://germ.example/a");
                    ] );
              ] );
          ( "viewer",
            `Assoc [ ("muted", `Bool false); ("mutedOnlyReposts", `Bool true) ]
          );
        ])
  in
  assert (profile_view.pronouns = Some "she/her");
  assert (profile_view.viewer.muted_only_reposts = Some true);
  (match profile_view.associated with
  | Some a -> (
      match a.germ with
      | Some g -> assert (g.show_button_to = "everyone")
      | None -> assert false)
  | None -> assert false);
  let sess =
    Session.parse_session_request
      (`Assoc
        [
          ("handle", `String "alice.test");
          ("did", `String "did:plc:abc123xyz0001112223333");
          ("active", `Bool false);
          ("status", `String "takendown");
        ])
  in
  assert (sess.email = None);
  assert (sess.status = Some "takendown");
  let schema =
    Records.lexicon_schema ~id:"com.example.ping"
      ~defs:(`Assoc [ ("main", `Assoc [ ("type", `String "query") ]) ])
      ()
  in
  assert (
    match Yojson.Safe.Util.member "$type" schema with
    | `String "com.atproto.lexicon.schema" -> true
    | _ -> false);
  let v2 =
    Feed.parse_search_posts_v2
      (`Assoc
        [
          ("posts", `List []);
          ("hitsTotal", `Int 0);
          ("detectedQueryLanguages", `List [ `String "ja" ]);
        ])
  in
  assert (v2.detected_query_languages = [ "ja" ]);
  let membership =
    Graph.parse_lists_with_membership
      (`Assoc [ ("listsWithMembership", `List []) ])
  in
  assert (membership.lists = []);
  let handle_check =
    Temp.parse_handle_check
      (`Assoc
        [
          ("handle", `String "available.test");
          ( "result",
            `Assoc
              [
                ( "$type",
                  `String
                    "com.atproto.temp.checkHandleAvailability#resultAvailable"
                );
              ] );
        ])
  in
  (match handle_check.result with `Available -> () | _ -> assert false);
  let signup =
    Temp.parse_signup_queue
      (`Assoc [ ("activated", `Bool true); ("placeInQueue", `Int 1) ])
  in
  assert signup.activated;
  let reserved = Temp.add_reserved_handle_body ~handle:"admin.bsky.social" () in
  assert (
    match Yojson.Safe.Util.member "handle" reserved with
    | `String "admin.bsky.social" -> true
    | _ -> false);
  let queue_body =
    Ozone.create_queue_body ~name:"spam" ~subject_types:[ "account" ] ()
  in
  assert (
    match Yojson.Safe.Util.member "name" queue_body with
    | `String "spam" -> true
    | _ -> false);
  (match
     Ozone.parse_report_activity
       (`Assoc [ ("$type", `String "tools.ozone.report.defs#noteActivity") ])
   with
  | `Note -> ()
  | _ -> assert false);
  let resolved =
    Lexicon.parse_resolved_lexicon
      (`Assoc
        [
          ( "uri",
            `String
              "at://did:plc:x/com.atproto.lexicon.schema/app.bsky.feed.post" );
          ("cid", `String "bafy");
          ( "schema",
            `Assoc
              [
                ("lexicon", `Int 1);
                ("id", `String "app.bsky.feed.post");
                ("defs", `Assoc []);
              ] );
        ])
  in
  assert (resolved.cid = "bafy");
  let list =
    Records.list ~name:"Friends" ~purpose:Records.purpose_curatelist
      ~created_at:"2024-01-01T00:00:00.000Z" ()
  in
  assert (
    match Yojson.Safe.Util.member "$type" list with
    | `String "app.bsky.graph.list" -> true
    | _ -> false);
  let pack =
    Records.starterpack ~name:"Start"
      ~list:"at://did:plc:abc123xyz0001112223333/app.bsky.graph.list/3k"
      ~created_at:"2024-01-01T00:00:00.000Z" ()
  in
  assert (
    match Yojson.Safe.Util.member "$type" pack with
    | `String "app.bsky.graph.starterpack" -> true
    | _ -> false);
  (match
     Feed.parse_thread_feed
       (`Assoc
         [
           ( "thread",
             `Assoc
               [
                 ("$type", `String "app.bsky.feed.defs#notFoundPost");
                 ( "uri",
                   `String
                     "at://did:plc:abc123xyz0001112223333/app.bsky.feed.post/3k"
                 );
                 ("notFound", `Bool true);
               ] );
         ])
   with
  | { thread = `NotFound n; _ } -> assert n.not_found
  | _ -> assert false);
  let chat_prefs =
    Chat.parse_notification_preferences
      (`Assoc
        [
          ("chat", `Assoc [ ("include", `String "all"); ("push", `Bool true) ]);
          ( "chatRequest",
            `Assoc [ ("include", `String "follows"); ("push", `Bool false) ] );
        ])
  in
  assert chat_prefs.chat.push;
  let actor_st =
    Chat.parse_actor_status
      (`Assoc
        [
          ("chatDisabled", `Bool false);
          ("canCreateGroups", `Bool true);
          ("groupMemberLimit", `Int 50);
        ])
  in
  assert (actor_st.group_member_limit = 50);
  let sys =
    Chat.parse_message
      (`Assoc
        [
          ("$type", `String "chat.bsky.convo.defs#systemMessageView");
          ("id", `String "s1");
          ("rev", `String "r1");
          ("sentAt", `String "2026-01-01T00:00:00.000Z");
          ( "data",
            `Assoc
              [
                ( "$type",
                  `String "chat.bsky.convo.defs#systemMessageDataUnlockConvo" );
                ( "unlockedBy",
                  `Assoc [ ("did", `String "did:plc:abc123xyz0001112223333") ]
                );
              ] );
        ])
  in
  assert sys.is_system;
  (match sys.system with
  | Some (`Unlock u) -> assert (String.length u.did > 0)
  | _ -> assert false);
  let reqs =
    Chat.parse_convo_requests
      (`Assoc
        [
          ( "requests",
            `List
              [
                `Assoc
                  [
                    ( "$type",
                      `String "chat.bsky.group.defs#joinRequestConvoView" );
                    ("convoId", `String "g1");
                    ("name", `String "Friends");
                    ( "owner",
                      `Assoc
                        [ ("did", `String "did:plc:abc123xyz0001112223333") ] );
                    ("memberCount", `Int 2);
                    ("memberLimit", `Int 50);
                  ];
              ] );
        ])
  in
  (match reqs.requests with
  | [ `Join_request jr ] -> assert (jr.member_limit = 50)
  | _ -> assert false);
  let decl = Records.chat_declaration ~allow_incoming:"following" () in
  assert (
    match Yojson.Safe.Util.member "$type" decl with
    | `String "chat.bsky.actor.declaration" -> true
    | _ -> false);
  let tagged =
    Unspecced.parse_tagged_suggestions
      (`Assoc
        [
          ( "suggestions",
            `List
              [
                `Assoc
                  [
                    ("tag", `String "news");
                    ("subjectType", `String "feed");
                    ( "subject",
                      `String
                        "at://did:plc:z72i7hdynmk6r22z27h6tvur/app.bsky.feed.generator/whats-hot"
                    );
                  ];
              ] );
        ])
  in
  assert (List.length tagged.suggestions = 1);
  let thread_v2 =
    Unspecced.parse_thread_v2
      (`Assoc
        [
          ("hasOtherReplies", `Bool false);
          ( "thread",
            `List
              [
                `Assoc
                  [
                    ( "uri",
                      `String
                        "at://did:plc:abc123xyz0001112223333/app.bsky.feed.post/3k"
                    );
                    ("depth", `Int 0);
                    ( "value",
                      `Assoc
                        [
                          ( "$type",
                            `String "app.bsky.unspecced.defs#threadItemNotFound"
                          );
                        ] );
                  ];
              ] );
        ])
  in
  assert (List.length thread_v2.thread = 1);
  let draft_body =
    Draft.draft_json
      ~posts:
        [
          {
            text = "draft";
            labels = None;
            embed_images = [];
            embed_gallery = None;
            embed_videos = [];
            embed_externals = [];
            embed_records = [];
          };
        ]
      ()
  in
  assert (
    match Yojson.Safe.Util.member "posts" draft_body with
    | `List xs -> List.length xs = 1
    | _ -> false);
  let aa =
    Ageassurance.parse_state
      (`Assoc [ ("status", `String "unknown"); ("access", `String "unknown") ])
  in
  assert (aa.status = "unknown");
  let contact_status = Contact.parse_sync_status_opt (`Assoc []) in
  assert (contact_status.sync_status = None);
  (match
     Actor.parse_preferences
       (`Assoc
         [
           ( "preferences",
             `List
               [
                 `Assoc
                   [
                     ("$type", `String "app.bsky.actor.defs#mutedWordsPref");
                     ("items", `List []);
                   ];
               ] );
         ])
   with
  | { preferences = [ { kind = `Muted_words _; _ } ]; _ } -> ()
  | _ -> assert false);
  assert (String.length (Chat.subscribe_mod_events_url ()) > 20);
  assert (
    Xrpc.proxy_to_string (Chat.effective_proxy ())
    = "did:web:api.bsky.chat#bsky_chat");
  let member_leftover =
    Chat.parse_member
      (`Assoc
        [
          ("did", `String "did:plc:abc123xyz0001112223333");
          ("handle", `String "alice.test");
          ("avatar", `String "https://cdn.example/a.jpg");
          ("createdAt", `String "2024-01-01T00:00:00.000Z");
        ])
  in
  assert (member_leftover.avatar <> None);
  (match
     (Chat.parse_message
        (`Assoc
          [
            ("id", `String "m1");
            ("rev", `String "r1");
            ("text", `String "reply");
            ("sentAt", `String "2024-01-01T00:00:00.000Z");
            ( "replyTo",
              `Assoc
                [
                  ( "$type",
                    `String
                      "chat.bsky.convo.defs#messageBeforeUserJoinedGroupView" );
                ] );
          ]))
       .reply_to
   with
  | Some `Before_join -> ()
  | _ -> assert false);
  let ozone_cfg =
    Ozone.parse_server_config
      (`Assoc
        [
          ("appview", `Assoc [ ("url", `String "https://appview.example") ]);
          ("blobDivert", `Assoc [ ("url", `String "https://divert.example") ]);
          ("verifierDid", `String "did:plc:verifier000111222333444555");
          ( "viewer",
            `Assoc [ ("role", `String "tools.ozone.team.defs#roleAdmin") ] );
        ])
  in
  assert (ozone_cfg.blob_divert <> None);
  assert (ozone_cfg.verifier_did <> None);
  (match
     Ozone.parse_subject
       (`Assoc
         [
           ("$type", `String "com.atproto.admin.defs#repoRef");
           ("did", `String "did:plc:abc123xyz0001112223333");
         ])
   with
  | `Repo_ref r -> assert (r.did <> "")
  | _ -> assert false);
  let lex_docs = Lexicon.official_documents () in
  assert (List.length lex_docs >= 5);
  assert (
    match Yojson.Safe.Util.member "$type" post with
    | `String "app.bsky.feed.post" -> true
    | _ -> false);
  (match
     Notification.parse_record
       (`Assoc
         [
           ("$type", `String "app.bsky.feed.post");
           ("text", `String "quote");
           ("createdAt", `String "2024-01-01T00:00:00.000Z");
         ])
       "quote"
   with
  | `Quote q -> assert (q.text = "quote")
  | _ -> assert false);
  let did_res =
    Identity.parse_did_resolution
      (`Assoc
        [
          ( "didDoc",
            `Assoc
              [
                ("id", `String "did:plc:abc123xyz0001112223333");
                ("alsoKnownAs", `List []);
                ("verificationMethod", `List []);
                ("service", `List []);
              ] );
        ])
  in
  (match did_res.document with
  | Some doc -> assert (doc.id = "did:plc:abc123xyz0001112223333")
  | None -> assert false);
  let rec_get =
    Repo.parse_record_get
      (`Assoc
        [
          ( "uri",
            `String
              "at://did:plc:abc123xyz0001112223333/app.bsky.feed.post/3jzfcijpj2z2a"
          );
          ("cid", `String "bafyreihdummy");
          ("value", post);
        ])
  in
  assert (String.length rec_get.uri > 8);
  let lex =
    Lexicon.of_string
      {|{"lexicon":1,"id":"com.example.ping","defs":{"main":{"type":"query","parameters":{"type":"params","properties":{}}}}}|}
  in
  assert (lex.id = "com.example.ping");
  assert (Http_method.to_string Http_method.Get = "GET");
  assert (Hash.sha256_hex "abc" <> "");
  let n, _ = Varint.decode (Varint.encode 128) in
  assert (n = 128);
  assert (Syntax.is_valid_nsid "app.bsky.video.uploadVideo");
  let writes =
    Repo.apply_writes_body ~repo:"did:plc:abc123xyz0001112223333"
      ~writes:
        [
          Repo.Delete
            { collection = "app.bsky.feed.like"; rkey = "3jzfcijpj2z2a" };
        ]
      ()
  in
  assert (
    match Yojson.Safe.Util.member "repo" writes with
    | `String _ -> true
    | _ -> false);
  let describe =
    Server.parse_describe_server
      (`Assoc
        [
          ("did", `String "did:web:bsky.social");
          ("availableUserDomains", `List [ `String ".bsky.social" ]);
        ])
  in
  assert (describe.did = "did:web:bsky.social");
  let proxy = Xrpc.parse_proxy "did:web:api.bsky.chat#bsky_chat" in
  assert (proxy.service = "bsky_chat");
  let scopes = Oauth_scope.parse "atproto repo:app.bsky.feed.post" in
  Oauth_scope.require_atproto scopes;
  let ev =
    Jetstream.parse_event
      (`Assoc
        [
          ("$type", `String "message");
          ( "payload",
            `Assoc
              [
                ( "$type",
                  `String "network.bsky.jetstream.subscribeEvents#commit" );
                ("did", `String "did:plc:abc123xyz0001112223333");
                ("seq", `Int 1);
                ("time", `String "2026-01-01T00:00:00.000000Z");
                ("operation", `String "create");
                ("collection", `String "app.bsky.feed.post");
                ("rkey", `String "3jzfcijpj2z2a");
                ("rev", `String "3jzfcijpj2z2a");
              ] );
        ])
  in
  (match ev with `Commit _ -> () | _ -> assert false);
  let aud = Syntax.parse_did_ref "did:web:video.bsky.app#bsky_transcode" in
  assert (aud.fragment = Some "bsky_transcode");
  let blob_cid = Cid.of_blob "clip-bytes" in
  assert (blob_cid.codec = Cid.Raw);
  let acct =
    Repo_sync.create_account ~did:"did:plc:abc123xyz0001112223333"
      ~collections:[ "app.bsky.feed.post" ] ()
  in
  assert (acct.status = Repo_sync.Desynchronized);
  assert (Syntax.is_valid_repo_path "app.bsky.feed.post/3jzfcijpj2z2a");
  assert (Cid.is_blessed (Cid.create "{\"v\":1}"));
  let start, finish = Mst.collection_range "app.bsky.feed.post" in
  assert (start = "app.bsky.feed.post/" && finish = "app.bsky.feed.post0");
  let site_doc =
    Site.document ~site:"https://standard.site" ~title:"Notes"
      ~published_at:"2026-01-01T00:00:00.000Z" ()
  in
  assert (
    match Yojson.Safe.Util.member "$type" site_doc with
    | `String "site.standard.document" -> true
    | _ -> false);
  let germ =
    Germnetwork.declaration ~version:"1.0.0" ~current_key:"key-bytes" ()
  in
  assert (
    match Yojson.Safe.Util.member "$type" germ with
    | `String "com.germnetwork.declaration" -> true
    | _ -> false);
  let signing =
    Admin.update_account_signing_key_body ~did:"did:plc:abc123xyz0001112223333"
      ~signing_key:"did:key:z6Mkexample" ()
  in
  assert (
    match Yojson.Safe.Util.member "did" signing with
    | `String _ -> true
    | _ -> false);
  (match Embed.join_link ~code:"join-1" () with
  | `JoinLink e -> assert (e.code = "join-1")
  | _ -> assert false);
  let req =
    Request.get
      (Http_client.xrpc_url ~host:"public.api.bsky.app"
         "com.atproto.identity.resolveHandle"
         ~query:[ ("handle", "bsky.app") ]
         ())
      ()
  in
  assert (req.url <> "");
  let parsed = Http_client.parse_url req.url in
  assert (parsed.host = "public.api.bsky.app");
  let fake = Response.of_string ~status_code:200 "{}" in
  assert fake.success;
  let confirm = Server.confirm_email_body ~email:"a@b.test" ~token:"t" in
  assert (
    match Yojson.Safe.Util.member "token" confirm with
    | `String "t" -> true
    | _ -> false);
  let jss_hdr =
    Jetstream.Jss.parse_header
      (Jetstream.Jss.encode_header
         {
           checksum = 0L;
           version = 1;
           block_count = 0;
           event_count = 0;
           unique_did_count = 0;
           min_seq = 0L;
           max_seq = 0L;
           min_witnessed_at = 0L;
           max_witnessed_at = 0L;
           footer_offset = 0L;
           did_bloom_offset = 0L;
           block_did_bloom_offset = 0L;
           collection_index_offset = 0L;
           block_index_offset = 0L;
           sealed = false;
         })
  in
  assert (jss_hdr.version = 1);
  let optout =
    Records.referencelistoptout
      ~subject:"at://did:plc:abc123xyz0001112223333/app.bsky.graph.list/3k"
      ~created_at:"2024-01-01T00:00:00.000Z" ()
  in
  assert (
    match Yojson.Safe.Util.member "$type" optout with
    | `String "app.bsky.graph.referencelistoptout" -> true
    | _ -> false);
  let par =
    Oauth.pushed_authorization_body
      ~client_id:"https://client.example/client-metadata.json"
      ~redirect_uri:"https://client.example/cb"
      ~code_challenge:"E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM" ~state:"s"
      ~prompt:"create" ()
  in
  assert (List.assoc_opt "prompt" par = Some "create");
  let _revoke =
    Oauth.revoke_body ~client_id:"https://client.example/client-metadata.json"
      ~token:"access" ~token_type_hint:"access_token" ()
  in
  assert (Oauth_scope.is_official_include "app.bsky.authCreatePosts");
  let set =
    Lexicon.parse_permission_set
      (Yojson.Safe.from_string
         (List.assoc "app.bsky.authCreatePosts" Lexicon.official_lexicons))
  in
  assert (List.length set.permissions = 2);
  print_endline "examples/offline: public API typechecks and fixtures pass"
