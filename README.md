# atproto

OCaml toolkit for the [AT Protocol](https://atproto.com) (XRPC, lexicons, repo sync, identity, AppView, Ozone, chat).

## Environment

Create a `.env` (see `sample.env`) with at least:

- `ATP_AUTH` : `EmailAddress:AppPassword`
  - Use an [App Password](https://bsky.app/settings/app-passwords) (email as the username).
- `ATP_HOST` : `bsky.social`
  - PDS / entryway host **without** a scheme.

Optional:

- `BASE_ENDPOINT` : `xrpc` (default)

Session creation, repo writes, graph mutes, bookmarks, chat, ozone, and feed helpers need `ATP_AUTH`. Public identity, DID PLC, firehose subscribe, AppView reads (`public.api.bsky.app`), and most `com.atproto.sync.*` reads do **not**.

## Build and test

```shell
opam install . --deps-only --with-test
dune build
dune runtest
```

`dune build` also typechecks `examples/offline.ml` against the current public API (no network, no credentials).

Live Bluesky tests that need credentials are skipped unless `ATP_AUTH` is set to a real `email:app-password` pair (placeholder values in `sample.env` do not count). Public-network tests (handle resolve, PLC directory, `getLatestCommit`, `subscribeRepos`, AppView feed/search/labeler reads) run without auth and skip only if the request itself fails.

## What this library covers

| Area | Module | Notes |
| --- | --- | --- |
| Session / JWT | `Auth`, `Session` | `createSession` URL uses `ATP_HOST` + `BASE_ENDPOINT` |
| AppView actor | `Actor` | Profiles, search, suggestions, get/put preferences (all current `app.bsky.actor.defs#preferences` kinds) |
| AppView feed | `Feed` | Timeline, `getPostThread` (`threadViewPost` / `notFoundPost` / `blockedPost`, optional parent, top-level embed + quote/bookmark counts), generators, `searchPosts` + `searchPostsV2` (array filters, `detectedQueryLanguages`), quotes, list feed, interactions |
| AppView graph | `Graph` | Follows/blocks/mutes, lists, starter packs, `searchStarterPacks` + `searchStarterPacksV2`, `getListsWithMembership` / `getStarterPacksWithMembership`, relationships, known followers |
| Bookmarks | `Bookmark` | `createBookmark` / `deleteBookmark` / `getBookmarks`; bookmark `item` is the feed `#postView` / `#notFoundPost` / `#blockedPost` union |
| Jetstream | `Jetstream` | v2 live tail, collection/DID/kind filters, seq + unix-µs cursors, reconnect/dedupe, v1 `/subscribe` compat; Network Replay planner + skippable unauthenticated HTTP (no invented archive token); `.jss` v1 header / block-index / columnar decode (zstd via injected callback) |
| Video | `Video` | `getJobStatus`, `getUploadLimits`, byte upload (`uploadVideo` URL + POST), multipart `startUpload` / `uploadPart` / `finishUpload` / `abortUpload` / `getUploadStatus`, service-auth audience (`did:web:<pds>` + `uploadBlob` lxm), injectable job poll, `video_embed_json`. Client only — no hosted transcoder |
| Unspecced | `Unspecced` | Popular generators, search skeletons, trending topics + `getTrends` / `getTrendsSkeleton`, tagged suggestions, unspecced age-assurance state, suggestion / feed / starter-pack / onboarding / discover / explore / seeMore skeletons, `getPostThreadV2` / `getPostThreadOtherV2`, config |
| Labeler | `Labeler` | `app.bsky.labeler.getServices` |
| Chat / DMs | `Chat` | `chat.bsky.convo.*` including typed message facets/reactions/embeds, lock/unlock, `getConvoMembers`; `chat.bsky.group` create/add/remove/edit + join links / join requests / mutual groups; notification prefs; actor status / declaration / export / delete; moderation views + `subscribeModEvents`; `atproto-proxy: did:web:api.bsky.chat#bsky_chat` |
| Ozone | `Ozone` | `tools.ozone.moderation.*` including typed event/subject unions, subjects/repos/records, timeline, reporter stats, scheduled actions; plus communication templates, sets, settings, team, safelink, signature, verification, hosting history + `getConfig`; `tools.ozone.queue.*` (list/create/update/delete, moderator assign, `routeReports`) and `tools.ozone.report.*` (query/get, activities, assignments, stats, close/reassign); requires `atproto-proxy` |
| Admin | `Admin` | `com.atproto.admin` subject status, account info, invites, email |
| Repo writes | `Repo`, `Records` | `createRecord` / `putRecord` / `deleteRecord` / `applyWrites` bodies; typed `describeRepo` / `getRecord` / `listRecords` parsers; builders for post/like/repost/follow/block/listblock/list/listitem/starterpack/profile/status/contentVisibility/verification/threadgate/postgate/generator/labeler/notification declaration |
| Server | `Server` | describe server (typed), app passwords, invites, `reserveSigningKey`, account activate/status, `getServiceAuth` (aud may be `did#service`) |
| Identity | `Identity`, `Did_plc`, `Did_web`, `Did_key` | resolve + typed `resolveDid` DID document + updateHandle / PLC operation helpers + `refreshIdentity` |
| PLC chain | `Did_plc` | Genesis DID, prev CID links, p256 **and k256** ECDSA (low-S, IEEE P1363) |
| Sync | `Sync` | `getLatestCommit`, `getRepo` (CAR), public `getBlocks` (bytes/CAR), `listBlobs`, `listRepos`, host/repo status |
| CID / CAR | `Cid`, `Car`, `Dag_cbor` | CIDv1 (including SHA-256 `Cid.create`) + CARv1, blessed CID check, Sync 1.1 streamable pre-order |
| MST | `Mst` | Layer/prefix rules, node parse, CID verify, lookup, insert/delete/walk, firehose-diff inversion **and** forward apply, p256/k256 commit sign+verify, pre-order blocks, collection-range proofs |
| Repo sync (TAP-like) | `Repo_sync` | Library-shaped backfill: open/verify repo CAR, walk records, `getRecord` inclusion proof (partial CAR), record-table apply of firehose ops, `#sync` desync, MST-level `apply_commit_tree`, Sync 1.1 pre-order export + collection-subset CAR. Not a hosted Tap service |
| TID | `Tid` | Record-key / commit-rev identifiers (base32-sortable, official syntax) |
| AT URI | `At_uri` | `at://` parse / serialize |
| Lexicon | `Lexicon` | Parse lexicon-1 JSON (parameters + procedure input/output schemas), `to_ocaml` codegen (unions emit polymorphic variants), JSON validate, `resolveLexicon` client, small bundled official lexicon documents |
| Temp | `Temp` | `com.atproto.temp.checkHandleAvailability` (available / suggestions union), `checkSignupQueue`, `dereferenceScope`, plus privileged `addReservedHandle` / `requestPhoneVerification` / `revokeAccountCredentials` clients (no invented operator session). Deprecated `fetchLabels` remains `Label.query_labels` |
| Firehose | `Firehose`, `Websocket` | RFC 6455 client + `subscribeRepos` frame decode (`#commit`/`#sync`/`#identity`/`#account`/`#info`) |
| OAuth / DPoP | `Oauth`, `Oauth_scope` | PKCE S256, DPoP ES256 + nonce, client metadata, PAR/token loop; granular scope grammar (`repo:`/`rpc:`/`blob:`/`include:`/`transition:`) |
| Labels | `Label` | `queryLabels` + label / query parse (`ver`, `exp`) + `#selfLabels` |
| XRPC headers | `Xrpc` | `atproto-proxy`, accept-labelers, rate-limit; service-auth JWT mint/verify (ES256/ES256K, `kid`/`jti`/`iat`/`lxm`, `did#service` aud, replay cache) |
| Errors | `Error` | XRPC `{error, message}` including rate limits |
| Syntax | `Syntax` | Handle, DID, NSID, record-key, datetime, language validators |
| Drafts | `Draft` | `app.bsky.draft` create/get/update/delete + typed draft / embed / threadgate / postgate builders |
| Contacts | `Contact` | `app.bsky.contact` phone verify, import, matches, dismiss, sync status, remove data |
| Age assurance | `Ageassurance` | `app.bsky.ageassurance` begin / getConfig / getState + region-rule union |
| Embeds / facets | `Embed`, `Facet` | Images, external, record, recordWithMedia, video, **gallery**, record `#view` union; `getEmbedExternalView`; mention / link / tag parse **and serialize** |
| Notifications | `Notification` | All `listNotifications` known reasons; prefs / prefs-v2 / activity subscriptions / register+unregister push |
| User reports | `Moderation` | `com.atproto.moderation.createReport` (strongRef / repoRef, optional `modTool`, reason-type constants) |
| Crypto / codecs | `K256`, `Base32`, `Base58`, `Base64url`, `Hash`, `Varint` | secp256k1, multibase, CID/CAR varints |
| HTTP helpers | `App`, `Client`, `Cohttp_client`, `Http_client`, `Http_method`, `Request`, `Response`, `User` | Endpoint URLs, shared XRPC GET/POST, method enum |

## Remaining gaps

These are product-level, not missing protocol cores:

- Hosting a public **client-metadata document** and completing a **live browser login** against a PDS (the protocol core — metadata, PAR + DPoP-nonce retry, authorize URL, redirect `code`/`state`/`iss`, token parse — is implemented and tested with fixtures)
- A hosted PDS, hosted Tap service, hosted video transcoder, or live Ozone operator session (client request/response types, video byte-upload + job poll, TAP-like repo sync helpers, and proxy headers are implemented)
- Jetstream Network Replay / HTTP snapshot **download** against Bluesky's gated archive (planner, `listSegments` types, cutover cursor, Range resume, skippable unauthenticated HTTP, and `.jss` v1 decode are implemented; a live archive download still needs an operator token this library does not invent, and zstd frames need an injected decompressor)
- Permissioned data / spaces / LtHash (no stable public spec to implement yet)
- Official `com.atproto.sync.getRepo` **lexicon** still has no `collection` parameter (client-side subset export from a full CAR is implemented; servers that reject unknown query params are unchanged)

#70–#81 covered protocol core, AppView/chat/ozone, Jetstream, video, OAuth scopes, and thread v2 / drafts / contacts / remaining preference kinds.

This stack fills remaining *library* XRPC vs current public lexicons: `tools.ozone.queue.*`, `tools.ozone.report.*`, and the remaining testable `com.atproto.temp.*` procedures/queries (`checkSignupQueue`, `dereferenceScope`, reserved-handle / phone-verification / revoke-credentials clients). Privileged Ozone/temp writes still need a real operator session and are not invented here.

Still leftover vs current lexicons (not invented here):

- Deprecated `com.atproto.temp.fetchLabels` (use `Label.query_labels` / `subscribeLabels`)
- `app.bsky.auth*` / `chat.bsky.authFullChatClient` permission documents (OAuth scope tokens, not XRPC clients)
- `Http_client` H2 stub and unused `Request`/`Response` types

Open PR `#69` (`de-sync-types`) is superseded by this work: it still targeted the removed `getCheckout` API and left CAR/CBOR unfinished.

## Sample usage

```ocaml
(* public AppView, no auth *)
let did = (Identity.resolve_handle "jay.bsky.team").did
let commit = Sync.get_latest_commit did
let discover =
  Feed.get_feed_generator
    ~feed:"at://did:plc:z72i7hdynmk6r22z27h6tvur/app.bsky.feed.generator/whats-hot"
    ()
let posts = Feed.search_posts ~q:"atproto" ~limit:5 ()
let posts_v2 = Feed.search_posts_v2 ~query:"atproto" ~hashtags:[ "atproto" ] ~limit:5 ()
let packs = Graph.search_starter_packs_v2 ~q:"bluesky" ~limit:5 ()
let popular = Unspecced.get_popular_feed_generators ~limit:5 ()
let trends = Unspecced.get_trends ~limit:5 ()
let services =
  Labeler.get_services ~dids:[ "did:plc:ar7c4by46qjdydhdevvrndac" ] ()

(* MST layer for a repo key — official vector *)
let () = assert (Mst.layer_for_key "blue" = 1)

(* TID used as record keys and commit revs *)
let () = assert (Tid.is_valid "3jzfcijpj2z2a")

(* typed Bluesky record builders + facet serialize *)
let post =
  Records.post ~text:"hello #atproto" ~created_at:"2024-01-01T00:00:00.000Z"
    ~facets:[ Facet.tag ~byte_start:6 ~byte_end:14 "atproto" ]
    ()

(* OAuth client metadata + authorize URL (no hosted client required) *)
let meta =
  Oauth.public_metadata
    ~client_id:"https://client.example/client-metadata.json"
    ~redirect_uris:[ "https://client.example/cb" ] ()
let _ = Oauth.validate_metadata meta

(* video byte-upload pipeline — construct URL + embed; POST needs a service token *)
let upload =
  Video.upload_video_url ~did:"did:plc:abc123xyz0001112223333" ~name:"clip.mp4" ()
let embed =
  Video.video_embed_json
    ~video:(`Assoc [ ("$type", `String "blob"); ("mimeType", `String "video/mp4") ])
    ~alt:"demo" ()
let start =
  Video.start_upload_body ~size_bytes:1_048_576 ~mime_type:"video/mp4"
    ~name:"clip.mp4" ()

(* firehose: one subscribeRepos frame from the public relay *)
let _header, msg = Firehose.subscribe_one ()

(* Jetstream v2 JSON tail — URL only here; subscribe_one talks to the public WS *)
let _js =
  Jetstream.subscribe_url
    ~filter:
      {
        Jetstream.empty_filter with
        collections = [ "app.bsky.feed.post" ];
        kinds = [ Jetstream.Commit ];
      }
    ()

(* TAP-like local indexer: backfill a CAR, then apply firehose ops *)
let acct =
  Repo_sync.create_account ~did:"did:plc:abc123xyz0001112223333"
    ~collections:[ "app.bsky.feed.post" ] ()
let _ = acct.Repo_sync.status

(* granular OAuth scopes (atproto remains mandatory) *)
let scopes = Oauth_scope.parse "atproto repo:app.bsky.feed.post"

(* authenticated writes / private surfaces *)
let username, password = Auth.username_and_password_from_env
let session = Session.create_session username password
let profile = Actor.get_profile session "jay.bsky.team"
let prefs = Actor.get_preferences session
let bookmarks = Bookmark.get_bookmarks session ~limit:10 ()
(* chat + ozone always need atproto-proxy (defaults shown) *)
let convos = Chat.list_convos session ~limit:10 ()
let chat_status = Chat.get_actor_status session ()
```
