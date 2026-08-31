(* Compiled (not live-run) examples for the public API documented in README.
   `dune build` typechecks this against the current modules. *)

open Atproto.Video
open Atproto.Embed
open Atproto.Facet
open Atproto.Lexicon
open Atproto.Tid
open Atproto.Mst
open Atproto.Oauth
open Atproto.Xrpc
open Atproto.Jetstream
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
