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

Live Bluesky tests that need credentials are skipped unless `ATP_AUTH` is set to a real `email:app-password` pair (placeholder values in `sample.env` do not count). Public-network tests (handle resolve, PLC directory, `getLatestCommit`, `subscribeRepos`, AppView feed/search/labeler reads) run without auth and skip only if the request itself fails.

## What this library covers

| Area | Module | Notes |
| --- | --- | --- |
| Session / JWT | `Auth`, `Session` | `createSession` URL uses `ATP_HOST` + `BASE_ENDPOINT` |
| AppView actor | `Actor` | Profiles, search, suggestions, get/put preferences |
| AppView feed | `Feed` | Timeline, threads, generators (`getFeed` / `getFeedGenerator` / `getActorFeeds`), `searchPosts`, quotes, list feed, interactions |
| AppView graph | `Graph` | Follows/blocks/mutes, lists, starter packs, relationships, known followers |
| Bookmarks | `Bookmark` | `createBookmark` / `deleteBookmark` / `getBookmarks` |
| Video | `Video` | `getJobStatus`, `getUploadLimits` |
| Unspecced | `Unspecced` | Popular generators, search skeletons, trending topics, config |
| Labeler | `Labeler` | `app.bsky.labeler.getServices` |
| Chat / DMs | `Chat` | `chat.bsky.convo.*` with `atproto-proxy: did:web:api.bsky.chat#bsky_chat` |
| Ozone | `Ozone` | `tools.ozone.moderation.*` + `getConfig`; requires `atproto-proxy` |
| Admin | `Admin` | `com.atproto.admin` subject status, account info, invites, email |
| Repo writes | `Repo` | `createRecord` / `putRecord` / `deleteRecord` / `applyWrites` bodies; `uploadBlob` parse |
| Server | `Server` | describe server (typed), app passwords, invites, `reserveSigningKey`, account activate/status |
| Identity | `Identity`, `Did_plc`, `Did_web`, `Did_key` | resolve + updateHandle / PLC operation helpers |
| PLC chain | `Did_plc` | Genesis DID, prev CID links, p256 **and k256** ECDSA (low-S, IEEE P1363) |
| Sync | `Sync` | Current `getLatestCommit`, `getRepo` (CAR), `listBlobs`, `listRepos` |
| CID / CAR | `Cid`, `Car`, `Dag_cbor` | CIDv1 (including SHA-256 `Cid.create`) + CARv1 |
| MST | `Mst` | Layer/prefix rules, node parse, CID verify, lookup, insert/delete, firehose-diff inversion, p256/k256 commit sign+verify |
| TID | `Tid` | Record-key / commit-rev identifiers (base32-sortable, official syntax) |
| AT URI | `At_uri` | `at://` parse / serialize |
| Lexicon | `Lexicon` | Parse lexicon-1 JSON, `to_ocaml` codegen, JSON validate |
| Firehose | `Firehose`, `Websocket` | RFC 6455 client + `subscribeRepos` frame decode (`#commit`/`#sync`/`#identity`/`#account`/`#info`) |
| OAuth / DPoP | `Oauth` | PKCE S256, DPoP ES256 + nonce, client metadata, AS/resource metadata, redirect callback, PAR/token loop (injectable HTTP) |
| Labels | `Label` | `queryLabels` + label / query parse (`ver`, `exp`) |
| XRPC headers | `Xrpc` | `atproto-proxy`, accept-labelers, rate-limit, service-auth JWT; chat + appview proxies |
| Errors | `Error` | XRPC `{error, message}` including rate limits |

## Remaining gaps

These are product-level, not missing protocol cores:

- Hosting a public **client-metadata document** and completing a **live browser login** against a PDS (the protocol core — metadata, PAR + DPoP-nonce retry, authorize URL, redirect `code`/`state`/`iss`, token parse — is implemented and tested with fixtures)
- A hosted PDS, video byte upload, or live Ozone operator session (client request/response types and proxy headers are implemented)

PLC k256 verify, MST firehose-diff inversion, signed-commit verify, OAuth client-metadata / PAR+token loop, and the AppView/Ozone/chat client surface are implemented in this stack (#70–#73 / this slice).

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

(* firehose: one subscribeRepos frame from the public relay *)
let _header, msg = Firehose.subscribe_one ()

(* authenticated writes / private surfaces *)
let username, password = Auth.username_and_password_from_env
let session = Session.create_session username password
let profile = Actor.get_profile session "jay.bsky.team"
let prefs = Actor.get_preferences session
let bookmarks = Bookmark.get_bookmarks session ~limit:10 ()
(* chat + ozone always need atproto-proxy (defaults shown) *)
let convos = Chat.list_convos session ~limit:10 ()
```
