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
| AppView actor | `Actor` | Profiles, search, suggestions, get/put preferences |
| AppView feed | `Feed` | Timeline, threads, generators (`getFeed` / `getFeedGenerator` / `getActorFeeds`), `searchPosts`, quotes, list feed, interactions |
| AppView graph | `Graph` | Follows/blocks/mutes, lists, starter packs, relationships, known followers |
| Bookmarks | `Bookmark` | `createBookmark` / `deleteBookmark` / `getBookmarks` |
| Jetstream | `Jetstream` | v2 live tail (`wss://jetstream.us-west/east.bsky.network/xrpc/network.bsky.jetstream.subscribeEvents`), collection/DID/kind filters, seq + unix-µs cursors, reconnect/dedupe, v1 `/subscribe` compat, snapshot/replay URL+plan types + skippable unauthenticated `try_plan_snapshot` (no invented archive token) |
| Video | `Video` | `getJobStatus`, `getUploadLimits`, byte upload (`uploadVideo` URL + POST), service-auth audience (`did:web:<pds>` + `uploadBlob` lxm), injectable job poll, `video_embed_json`. Client only — no hosted transcoder |
| Unspecced | `Unspecced` | Popular generators, search skeletons, trending topics, config |
| Labeler | `Labeler` | `app.bsky.labeler.getServices` |
| Chat / DMs | `Chat` | `chat.bsky.convo.*` with `atproto-proxy: did:web:api.bsky.chat#bsky_chat` |
| Ozone | `Ozone` | `tools.ozone.moderation.*` + `getConfig`; requires `atproto-proxy` |
| Admin | `Admin` | `com.atproto.admin` subject status, account info, invites, email |
| Repo writes | `Repo` | `createRecord` / `putRecord` / `deleteRecord` / `applyWrites` bodies; `uploadBlob` parse |
| Server | `Server` | describe server (typed), app passwords, invites, `reserveSigningKey`, account activate/status, `getServiceAuth` (aud may be `did#service`) |
| Identity | `Identity`, `Did_plc`, `Did_web`, `Did_key` | resolve + updateHandle / PLC operation helpers |
| PLC chain | `Did_plc` | Genesis DID, prev CID links, p256 **and k256** ECDSA (low-S, IEEE P1363) |
| Sync | `Sync` | `getLatestCommit`, `getRepo` (CAR), public `getBlocks` (bytes/CAR), `listBlobs`, `listRepos`, host/repo status |
| CID / CAR | `Cid`, `Car`, `Dag_cbor` | CIDv1 (including SHA-256 `Cid.create`) + CARv1 |
| MST | `Mst` | Layer/prefix rules, node parse, CID verify, lookup, insert/delete/walk, firehose-diff inversion **and** forward apply, p256/k256 commit sign+verify |
| Repo sync (TAP-like) | `Repo_sync` | Library-shaped backfill: open/verify repo CAR, walk records, `getRecord` inclusion proof, record-table apply of firehose ops, `#sync` desync, MST-level `apply_commit_tree`. Not a hosted Tap service |
| TID | `Tid` | Record-key / commit-rev identifiers (base32-sortable, official syntax) |
| AT URI | `At_uri` | `at://` parse / serialize |
| Lexicon | `Lexicon` | Parse lexicon-1 JSON (parameters + procedure input/output schemas), `to_ocaml` codegen, JSON validate |
| Firehose | `Firehose`, `Websocket` | RFC 6455 client + `subscribeRepos` frame decode (`#commit`/`#sync`/`#identity`/`#account`/`#info`) |
| OAuth / DPoP | `Oauth`, `Oauth_scope` | PKCE S256, DPoP ES256 + nonce, client metadata, PAR/token loop; granular scope grammar (`repo:`/`rpc:`/`blob:`/`include:`/`transition:`) |
| Labels | `Label` | `queryLabels` + label / query parse (`ver`, `exp`) |
| XRPC headers | `Xrpc` | `atproto-proxy`, accept-labelers, rate-limit; service-auth JWT mint/verify (ES256/ES256K, `kid`/`jti`/`iat`/`lxm`, `did#service` aud, replay cache) |
| Errors | `Error` | XRPC `{error, message}` including rate limits |
| Syntax | `Syntax` | Handle, DID, NSID, record-key, datetime, language validators |
| Embeds / facets | `Embed`, `Facet` | Images, external, record, recordWithMedia, video; mention / link / tag |
| Notifications | `Notification` | Unread count, list, updateSeen; unknown reasons parse as `` `Other `` |
| User reports | `Moderation` | `com.atproto.moderation.createReport` (strongRef / repoRef) |
| Crypto / codecs | `K256`, `Base32`, `Base58`, `Base64url`, `Hash`, `Varint` | secp256k1, multibase, CID/CAR varints |
| HTTP helpers | `App`, `Client`, `Cohttp_client`, `Http_client`, `Http_method`, `Request`, `Response`, `User` | Endpoint URLs, shared XRPC GET/POST, method enum |

## Remaining gaps

These are product-level, not missing protocol cores:

- Hosting a public **client-metadata document** and completing a **live browser login** against a PDS (the protocol core — metadata, PAR + DPoP-nonce retry, authorize URL, redirect `code`/`state`/`iss`, token parse — is implemented and tested with fixtures)
- A hosted PDS, hosted Tap service, hosted video transcoder, or live Ozone operator session (client request/response types, video byte-upload + job poll, TAP-like repo sync helpers, and proxy headers are implemented)
- Jetstream Network Replay / HTTP snapshot download against Bluesky's gated archive (plan/segment/block URLs, JSON types, and a skippable unauthenticated `try_plan_snapshot` are implemented; a live download still needs an operator token this library does not invent)
- Permissioned data / spaces / LtHash (no stable public spec to implement yet)
- Defined CAR block ordering and collection-subset repo exports (Sync 1.1 leftovers; no stable export layout to implement yet)

Service-auth JWT mint/verify (`jti`/`kid`/`iat`, `did#service` audience, ES256/ES256K), TAP-like `Repo_sync` (CAR walk, record-table apply, `#sync` resync, MST forward apply), blob CID verify, and skippable Jetstream snapshot HTTP are in this slice. #70–#75 already landed identity/MST/OAuth-core/AppView/Ozone/Jetstream/video.

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
let popular = Unspecced.get_popular_feed_generators ~limit:5 ()
let services =
  Labeler.get_services ~dids:[ "did:plc:ar7c4by46qjdydhdevvrndac" ] ()

(* MST layer for a repo key — official vector *)
let () = assert (Mst.layer_for_key "blue" = 1)

(* TID used as record keys and commit revs *)
let () = assert (Tid.is_valid "3jzfcijpj2z2a")

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
```
