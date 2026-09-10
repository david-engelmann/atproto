# atproto

**Typed OCaml toolkit for the [AT Protocol](https://atproto.com).**

Resolve identities, read and write repositories, follow the firehose, and call AppView, Ozone, and hosted Bluesky products (chat, video, Jetstream) from one library. Protocol pieces — XRPC, CID/CAR/MST, lexicons, OAuth/DPoP — are implemented here, not left as raw HTTP.

**1.0.2** is the packaged surface. Pin this repository until
**atproto.1.0.2** lands on opam-repository; then `opam install atproto`.
See [CHANGELOG.md](CHANGELOG.md).

This package is a **client**. It does not host a PDS, chat service, video transcoder, Tap, SMS gateway, or push backend. See [What this package does not host](#what-this-package-does-not-host).

## Quick start

These two calls hit the public AppView over the network. They do
not need `ATP_AUTH`. `ATP_PUBLIC` only gates live *tests*; it is
not required to run this snippet.

```shell
opam pin add atproto git+https://github.com/david-engelmann/atproto.git
# after atproto.1.0.2 is on opam-repository:
# opam install atproto
```

```ocaml
(* public AppView — needs network, no ATP_AUTH *)
let did = (Identity.resolve_handle "jay.bsky.team").did
let posts = Feed.search_posts ~q:"atproto" ~limit:5 ()
```

`examples/quickstart.ml` is that flow as a copy-paste executable (`dune exec -- examples/quickstart.exe`).

## Install

Requires OCaml **>= 4.14.1 and < 5.4** (CI: **4.14.1** and **5.3.0**). Jane Street `core` / `async` / `ppx_jane` / `zstandard` are **>= v0.16.0 and < v0.18~** (v0.16 on 4.14, v0.17 on 5.1–5.3). Public Jane Street v0.17 does not support OCaml 5.4+; 5.0 is untested. Jetstream dict-zstd needs system **libzstd** (Debian/Ubuntu `libzstd-dev`, macOS Homebrew `zstd`) before `opam pin` / `opam install . --deps-only`. The Jane Street `zstandard` package is Linux-only (x86_64 / arm64).

```shell
opam pin add atproto git+https://github.com/david-engelmann/atproto.git
# after atproto.1.0.2 is on opam-repository:
# opam install atproto
```

From a local clone:

```shell
opam pin add atproto .
# or install build/test deps without pinning a release
opam install . --deps-only --with-test
dune build -p atproto
```

In a dependent `dune` stanza:

```lisp
(libraries atproto)
```

`opam pin` / `opam install .` run `dune build -p atproto` and install the public `atproto` library. Release notes: [CHANGELOG.md](CHANGELOG.md). Official lexicons stay pinned at bluesky-social/atproto [`f0d4877a`](https://github.com/bluesky-social/atproto/commit/f0d4877a03dc8ede0d3e9a36d5b72ada63b5d2e0).

## Documentation

| Resource | Where |
| --- | --- |
| API reference | https://david-engelmann.github.io/atproto/ (`dune build @doc` / `make doc`) |
| Release notes | [CHANGELOG.md](CHANGELOG.md) |
| License | [LICENSE](LICENSE) |
| Issues | https://github.com/david-engelmann/atproto/issues |
| Security | [.github/SECURITY.md](.github/SECURITY.md) |
| Contributing | [.github/CONTRIBUTING.md](.github/CONTRIBUTING.md) |
| For agents | [AGENTS.md](AGENTS.md) |

Pushes to `main` deploy odoc with GitHub Actions Pages. Pull requests also upload the `odoc-html` artifact.

## Library map

Each row is a starting point. Function-level detail lives in [odoc](https://david-engelmann.github.io/atproto/); release-by-release depth is in the [CHANGELOG](CHANGELOG.md).

| Area | Modules | Purpose |
| --- | --- | --- |
| Session | `Auth`, `Session`, `Server` | App-password sessions, app passwords, invites, email, `getServiceAuth` |
| Identity | `Identity`, `Did_plc`, `Did_web`, `Did_key` | Handle / DID resolve, PLC directory, `did:web` / `did:key` |
| AppView | `Actor`, `Feed`, `Graph`, `Bookmark`, `Labeler`, `Unspecced` | Profiles, timelines, search, graphs, bookmarks, public labelers |
| Records | `Repo`, `Records`, `Embed`, `Facet` | create/put/delete/applyWrites and typed post/like/follow/… builders |
| Sync | `Sync`, `Repo_sync`, `Mst`, `Cid`, `Car`, `Dag_cbor` | Repo CAR, MST, CID; indexer / backfill toolkit (**not** a hosted Tap) |
| Firehose | `Firehose`, `Websocket`, `Jetstream` | `subscribeRepos` and Jetstream live tail / archive HTTP |
| OAuth | `Oauth`, `Oauth_scope` | PKCE, DPoP, public HTTPS client-metadata, browser login glue |
| Chat | `Chat` | Hosted `chat.bsky.*` client (`api.bsky.chat`). No OSS chat backend |
| Video | `Video` | Hosted `app.bsky.video.*` client (`video.bsky.app`). No transcoder |
| Ozone | `Ozone` | `tools.ozone.*` moderation client (PDS `atproto-proxy` or service-auth) |
| Notifications | `Notification` | List, prefs, activity subscriptions, `registerPush` (caller gateway) |
| Contacts | `Contact` | Hosted phone-verified contacts. No SMS gateway |
| Labels | `Label` | `queryLabels` / `subscribeLabels` and label-value definitions |
| Lexicon | `Lexicon` | Parse lexicon-1 JSON, validate, `to_ocaml` codegen |
| Syntax | `At_uri`, `Tid`, `Syntax`, `Error`, `Xrpc` | `at://`, TIDs, identifier validators, XRPC errors / headers |
| Other clients | `Admin`, `Temp`, `Moderation`, `Draft`, `Ageassurance` | Admin, temp, user reports, drafts, age assurance |
| Records (other) | `Site`, `Germnetwork` | `site.standard.*` and `com.germnetwork.declaration` builders |
| HTTP | `Client`, `Http_client`, `App` | Shared XRPC GET/POST; HTTP/2 TLS for public HTTPS |
| Experimental | `Lt_hash`, `At_uri.Space`, `Space_commit`, `Space_credential`, `Space_xrpc`, `Space_sync` | Proposal [0016](https://github.com/bluesky-social/proposals/blob/main/0016-permissioned-data/README.md) only — **not a spaces product API** |

## Environment

Create a `.env` (see `sample.env`) when you need a session or a non-default host.

| Variable | Purpose |
| --- | --- |
| `ATP_AUTH` | `EmailAddress:AppPassword` — use an [App Password](https://bsky.app/settings/app-passwords) |
| `ATP_HOST` | PDS / entryway host **without** a scheme (`bsky.social`; `localhost:2583` locally) |
| `ATP_SCHEME` | `https` (default) or `http` for a local stack without TLS |
| `ATP_PUBLIC` | Set `1` / `true` / `yes` / `on` to run unauthenticated public-internet live hops. Leave unset for offline `with-test`. |

Optional hosts (all without a scheme): `ATP_APPVIEW_HOST`, `ATP_OZONE_HOST` / `ATP_OZONE_DID`, `ATP_CHAT_HOST` / `ATP_CHAT_DID`, `ATP_VIDEO_HOST`. Local-network extras: `ATP_AUTH_BOB`, `ATP_AUTH_OZONE`, `BASE_ENDPOINT` (default `xrpc`).

Session creation, repo writes, graph mutes, bookmarks, chat, ozone, and most feed helpers need `ATP_AUTH`. Chat also needs a DM-capable session (`transition:chat.bsky`, `include:chat.bsky.authFullChatClient`, or `ATP_CHAT=1`). Public identity, DID PLC, firehose subscribe, AppView reads (`public.api.bsky.app`), and most `com.atproto.sync.*` reads do **not** need auth, but those live hops skip unless `ATP_PUBLIC` is truthy.

Live opt-ins (unset in CI): `ATP_PUBLIC` (unauthenticated public-internet hops), `ATP_CHAT`, `ATP_PHONE` / `ATP_PHONE_NUMBER`, `ATP_PUSH` / `ATP_PUSH_DID` / `ATP_PUSH_TOKEN`, `ATP_SPACE` / `ATP_SPACE_HOST` (no default space host), `JETSTREAM_API_KEY`.

## Hosted Bluesky products

These paths are library-ready. None of them start a hosted service in this repo. Offline sketches live under `examples/`.

### OAuth (HTTPS client-metadata)

AT Protocol identifies a public client by an HTTPS `client_id` that **is** the URL of a JSON metadata document. This library builds, validates, and serializes that document and drives the browser login path. It does **not** host the file or a login UI.

1. Build the document (`Oauth.public_https_metadata`). `client_id` must be `https://host/path` with no port; web `redirect_uris` must be HTTPS on the same origin. Native clients may use `http://127.0.0.1` / `http://[::1]` or a reverse-domain custom scheme.
2. Publish it at that URL as HTTP 200 `application/json` (`Oauth.metadata_document`). The body's `client_id` must match the fetch URL. See `examples/client-metadata.json` and `examples/oauth_https_metadata.ml`.
3. `Oauth.start_browser_login` discovers the PDS authorization server, runs PAR, and returns the authorize URL (PKCE S256 + DPoP + `state`).
4. On the redirect (`?code=&state=&iss=`), `Oauth.complete_browser_login` exchanges the code for a DPoP token. Authed AppView / Ozone / chat still use `Oauth.get_service_auth`, not the DPoP access token. **DPoP cannot be proxied.**

Loopback `http://localhost?redirect_uri=…` and local TestNetwork stay the development path. `Auth.createSession` stays a Cohttp password session.

### Chat (`chat.bsky.*`)

Client for the hosted Bluesky chat service (`did:web:api.bsky.chat#bsky_chat`, host `api.bsky.chat` / `ATP_CHAT_HOST`). Official TestNetwork does not start a DM service. This repo does not fake one.

- **Scopes.** `Oauth.default_scope` (`atproto transition:generic`) is not enough. Declare `Oauth.default_chat_scope` (adds `transition:chat.bsky`) or `Oauth_scope.full_chat_client_scope` (`include:chat.bsky.authFullChatClient`). Privileged app-passwords carry a chat grant; regular ones do not. `Chat.scope_has_chat` detects a DM grant.
- **Password session.** Privileged `createSession` + `Chat.list_convos` / `get_messages` / `send_message` through the PDS with `atproto-proxy` (`Chat.effective_proxy`).
- **OAuth.** Mint `getServiceAuth` (`aud` = `Chat.service_aud`, `lxm` = the `chat.bsky.*` NSID) and call `Chat.list_convos_service` / `get_messages_service` / `send_message_service` on `api.bsky.chat`. Those helpers do not send `atproto-proxy`.

`examples/chat_production.ml` is offline wiring.

### Video (`app.bsky.video.*`)

Client for the hosted Bluesky video service (`video.bsky.app` / `ATP_VIDEO_HOST`). Official TestNetwork does not start a transcoder. This repo does not fake one.

- **Service-auth.** Audience is `did:web:<pds-host>` from the session `#atproto_pds` (`Video.pds_audience`), not `did:web:video.bsky.app`. `lxm` is `com.atproto.repo.uploadBlob`. Password: `Video.mint_upload_token`. OAuth: `Oauth.get_service_auth` with the same aud / lxm.
- **Upload.** Small clips: `Video.upload_video`. Larger files: multipart `start_upload` / `upload_part` / `finish_upload`. Poll with `Video.poll_job_status` / `ensure_blob`.
- **Embed.** Put the job **blob ref** on the post (`Video.video_embed_json` / `embed_of_job` + `Records.post`). Do not write the HLS playlist into the create embed.

`examples/video_production.ml` is offline wiring.

### Indexer (`Repo_sync`)

A backfill / firehose-apply toolkit for building an indexer. It is **not** a hosted Tap. Official TestNetwork is a local PDS + AppView + Ozone stack, not a Tap host.

- **Backfill.** `Repo_sync.fetch_repo` / `backfill` pull `com.atproto.sync.getRepo`. Offline: `open_car` / `resync_from_car`.
- **Walk / proof.** `walk_json` decodes records as IPLD JSON. `export_record_proof` / `verify_record_proof` are getRecord inclusion proofs.
- **Firehose.** `process_commit` applies `#commit` ops while `Synchronized`. A `#sync` with a different rev marks `Desynchronized` until `resync_from_car`.
- **Export.** Sync 1.1 `export_car` / `export_subset`. Offline fixture: `write_signed_repo` (production signers use `Mst.sign_p256` / `sign_k256`).

`examples/repo_sync_indexer.ml` is an offline sketch.

### Jetstream

Client for Jetstream live tail and Network Replay HTTP. Live `subscribe` / `subscribe_one` stay unauthenticated (v2 offers `Sec-WebSocket-Protocol: xrpc.v1.json`; dict-zstd is `~compress:true`).

Bluesky-hosted archive HTTP (`planSnapshot` / `planBackfill` / `listSegments` / …) needs an operator API key from [bsky.network/account](https://bsky.network/account) — not a PDS JWT. Pass `JETSTREAM_API_KEY` (or `JETSTREAM_ARCHIVE_TOKEN` / `~token`) as the raw key. This library does not invent one. `require_archive_token` fails closed before HTTP. Self-hosted Jetstream needs no key.

`examples/jetstream_archive.ml` is offline wiring.

### Phone, contacts, and push

Client for hosted Bluesky phone verification, contact import, and push registration. It does not send SMS and does not start an APNs/FCM gateway.

- **Contacts.** Password sessions use `Contact.get_matches` / `import_contacts` through the PDS. OAuth mints AppView service-auth and calls `*_service` on `public.api.bsky.app`.
- **SMS.** `Contact.start_phone_verification` → `verify_phone` → `import_contacts` is Bluesky-hosted SMS. Live hops skip unless `ATP_PHONE=1` and `ATP_PHONE_NUMBER` is an E.164 number you own. `Temp.request_phone_verification` is a different privileged signup-SMS client and is also not faked.
- **Push.** `Notification.register_push` takes a caller `serviceDid`, device token, platform (`ios` / `android` / `web`), and `appId`. Official Bluesky push is closed to the official app.

`examples/contacts_production.ml` is offline wiring.

## What this package does not host

These are hosted products this client talks to, not missing protocol cores. The client paths above are implemented; the **servers** are not.

- **HTTPS `client-metadata.json` and the browser redirect** — your application hosts them. The library builds the document and drives authorize → code → token.
- **Tap** — `Repo_sync` is the indexer library. This repo does not fake a Tap host.
- **Video transcoder** — talk to `video.bsky.app`. No local transcoder.
- **OSS chat** — `@atproto/dev-env` 0.6.4 does not start one (`ozone.chatUrl` = `localhost:2590`, “must run separate chat service”). Talk to `api.bsky.chat`.
- **Jetstream archive key** — the operator supplies `JETSTREAM_API_KEY`. Live subscribe stays unauthenticated.
- **SMS / phone-verification gateway** — hosted Bluesky SMS only. `requestPhoneVerification` is not faked.
- **APNs/FCM** — official Bluesky push is closed to the official app; third-party clients host their own gateway.
- **Spaces** — `Lt_hash`, `At_uri.Space`, `Space_commit`, `Space_credential`, `Space_xrpc`, and `Space_sync` implement draft proposal [0016](https://github.com/bluesky-social/proposals/blob/main/0016-permissioned-data/README.md). They are **experimental**, not a stable product API. Deferred: `com.atproto.simplespace.*`, `space:` OAuth scopes, proposal `registerNotify` `repo`. Live hops skip unless `ATP_SPACE=1` and `ATP_SPACE_HOST` names a real host. This repo does not fake a space host.

Official lexicons are pinned at [`f0d4877a`](https://github.com/bluesky-social/atproto/commit/f0d4877a03dc8ede0d3e9a36d5b72ada63b5d2e0). CI `@lexicon-coverage` fails if that pin grows and a public client NSID lacks a helper (or an explicit skip). That gate is not `dune runtest` / opam `with-test`. Hosted-only *servers* are not skip reasons.

## Development

```shell
opam install . --deps-only --with-test
dune build
dune runtest
```

`dune build` typechecks `examples/offline.ml` against the public API (no network). `dune runtest` also runs the offline production sketches. A release-style build is `dune build -p atproto` and `dune runtest -p atproto`. Live Bluesky tests that need credentials skip unless `ATP_AUTH` is a real `email:app-password` pair (placeholders in `sample.env` do not count). Unauthenticated public-network tests (handle resolve, PLC, `getLatestCommit`, `subscribeRepos`, AppView reads) skip unless `ATP_PUBLIC` is truthy. Default GitHub TestSuite leaves `ATP_PUBLIC` unset so opam `with-test` stays offline-safe. Re-run public hops with `make test-public` or the optional **PublicLive** workflow (`workflow_dispatch` only).

### Local AT Protocol network

`make test-pds` starts Bluesky’s official [`@atproto/dev-env@0.6.4`](https://www.npmjs.com/package/@atproto/dev-env) — PLC (`:2582`), PDS (`:2583`), AppView (`:2584`), Ozone (`:2587`). Chat, video, Tap, SMS, and push are **not** in that stack.

```shell
make test-pds

# or step by step
./scripts/local-atproto.sh up
./scripts/local-atproto.sh account
eval "$(./scripts/local-atproto.sh env)"
export ATP_REQUIRE_LOCAL_PDS=1
dune exec -- test/test_local_pds.exe
./scripts/local-atproto.sh down
```

Point the client at the stack with `ATP_SCHEME=http`, `ATP_HOST=localhost:2583`, `ATP_APPVIEW_HOST=localhost:2584`, `ATP_OZONE_HOST=localhost:2587`, `ATP_AUTH=alice.test:hunter2`. Mock accounts come from official `generateMockSetup` (`alice.test` / `bob.test` / ozone admin `admin-mod.test`). If the network is up, a failed protocol call **fails the test**; the suite skips only when it is not aimed at a local host.

OAuth against this TestNetwork (loopback metadata, PAR, DPoP, service-auth) is covered by `test/test_local_oauth.ml`. See [.github/CONTRIBUTING.md](.github/CONTRIBUTING.md).

## Examples

Copy-paste sketches under `examples/`. `dune build` typechecks them;
none invent a hosted service.

| File | Demo |
| --- | --- |
| [`examples/quickstart.ml`](examples/quickstart.ml) | Public AppView: resolve a handle and search posts (no `ATP_AUTH`) |
| [`examples/offline.ml`](examples/offline.ml) | Typechecks the public API with no network |
| [`examples/oauth_https_metadata.ml`](examples/oauth_https_metadata.ml) | HTTPS `client-metadata.json` + browser login (you still host the document) |
| [`examples/client-metadata.json`](examples/client-metadata.json) | Sample public HTTPS OAuth client-metadata document (you still host it) |
| [`examples/chat_production.ml`](examples/chat_production.ml) | Hosted `chat.bsky.*` on `api.bsky.chat` (no OSS chat backend) |
| [`examples/video_production.ml`](examples/video_production.ml) | Hosted `app.bsky.video.*` on `video.bsky.app` (no transcoder) |
| [`examples/repo_sync_indexer.ml`](examples/repo_sync_indexer.ml) | Indexer / backfill via `Repo_sync` (not a Tap host) |
| [`examples/jetstream_archive.ml`](examples/jetstream_archive.ml) | Jetstream archive HTTP with an operator-supplied key |
| [`examples/contacts_production.ml`](examples/contacts_production.ml) | Hosted phone / contacts / push clients (no SMS or APNs/FCM) |

```ocaml
(* public AppView, no auth *)
let did = (Identity.resolve_handle "jay.bsky.team").did
let commit = Sync.get_latest_commit did
let posts = Feed.search_posts ~q:"atproto" ~limit:5 ()
let author =
  Feed.get_author_feed_page ~actor:"jay.bsky.team" ~limit:5
    ~filter:Feed.filter_posts_no_replies ~include_pins:true ()

(* typed record + facet *)
let post =
  Records.post ~text:"hello #atproto" ~created_at:"2024-01-01T00:00:00.000Z"
    ~facets:[ Facet.tag ~byte_start:6 ~byte_end:14 "atproto" ]
    ()

(* password session *)
let username, password = Auth.username_and_password_from_env
let session = Session.create_session username password
let profile = Actor.get_profile session "jay.bsky.team"

(* OAuth: you still host client-metadata.json at client_id and receive /cb *)
let client_id = Oauth.https_client_id ~host:"client.example" ()
let meta =
  Oauth.public_https_metadata ~client_id
    ~redirect_uris:[ "https://client.example/cb" ] ()
let _ = Oauth.metadata_http_response meta
(* Oauth.start_browser_login / complete_browser_login drive authorize → token *)

(* firehose + Jetstream URL (subscribe_one talks to the public WS) *)
let _header, msg = Firehose.subscribe_one ()
let _js =
  Jetstream.subscribe_url
    ~filter:
      {
        Jetstream.empty_filter with
        collections = [ "app.bsky.feed.post" ];
        kinds = [ Jetstream.Commit ];
      }
    ()

(* experimental proposal 0016 — not a spaces product *)
let space =
  At_uri.Space.of_string "at://did:example:space/space/app.bsky.group/test"
let () = assert (not (At_uri.Space.is_record space))
```
