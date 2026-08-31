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
  let embed = Video.video_embed_json ~video:blob ~alt:"demo" () in
  assert (
    match Yojson.Safe.Util.member "$type" embed with
    | `String "app.bsky.embed.video" -> true
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
    Draft.draft_json ~posts:[ { text = "draft"; labels = None; embed_images = [];
        embed_gallery = None; embed_videos = []; embed_externals = [];
        embed_records = [] } ] ()
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
  assert (
    String.length (Chat.subscribe_mod_events_url ()) > 20);
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
  print_endline "examples/offline: public API typechecks and fixtures pass"
