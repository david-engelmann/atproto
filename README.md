# atproto

Typed OCaml client for the [AT Protocol](https://atproto.com).

[![opam](https://img.shields.io/badge/opam-1.0.2-orange)](https://opam.ocaml.org/packages/atproto/)
[![docs](https://img.shields.io/badge/docs-odoc-informational)](https://david-engelmann.github.io/atproto/atproto/)
[![TestSuite](https://github.com/david-engelmann/atproto/actions/workflows/test_suite.yml/badge.svg)](https://github.com/david-engelmann/atproto/actions/workflows/test_suite.yml)

Resolve identities, read and write repositories, follow the firehose, and call AppView, Ozone, and hosted Bluesky services (chat, video, Jetstream). XRPC, CID/CAR/MST, lexicons, and OAuth/DPoP are in the library.

Install **1.0.2** from opam with `opam update && opam install atproto`. Pin this repo or a local clone for an unreleased tip. See [CHANGELOG.md](CHANGELOG.md).

This is a **client**. It doesn't host a PDS, chat service, video transcoder, Tap, SMS gateway, or push backend. See [What this package does not host](#what-this-package-does-not-host).

## Quick start

These two calls need the public network. They don't need `ATP_AUTH`. `Identity.resolve_handle` talks to the entryway (`ATP_HOST`, default `bsky.social`). `Feed.search_posts` talks to the public AppView (`public.api.bsky.app`). `ATP_PUBLIC` only gates live tests; you don't need it to run this snippet.

```shell
opam update
opam install atproto
```

```ocaml
(* public network — entryway resolve + AppView search; no ATP_AUTH *)
let did = (Identity.resolve_handle "jay.bsky.team").did
let posts = Feed.search_posts ~q:"atproto" ~limit:5 ()
```

`examples/quickstart.ml` is the same flow as an executable (`dune exec -- examples/quickstart.exe`).

## Install

The published package is [atproto on opam](https://opam.ocaml.org/packages/atproto/).
Requires OCaml **>= 4.14.1 and < 5.4** (CI: **4.14.1** and **5.3.0**). Jane Street `core` / `async` / `ppx_jane` / `zstandard` are **>= v0.16.0 and < v0.18~** (v0.16 on 4.14, v0.17 on 5.1–5.3). Public Jane Street v0.17 does not support OCaml 5.4+; 5.0 is untested. Jetstream dict-zstd needs system **libzstd** (Debian/Ubuntu `libzstd-dev`, macOS Homebrew `zstd`) before `opam install`. The Jane Street `zstandard` package is Linux-only (x86_64 / arm64).

```shell
opam update
opam install atproto
```

From a local clone, or an unreleased tip:

```shell
opam pin add atproto .
# or from GitHub:
# opam pin add atproto git+https://github.com/david-engelmann/atproto.git
# or install build/test deps without pinning a release
opam install . --deps-only --with-test
dune build -p atproto
```

In a dependent `dune` stanza:

```lisp
(libraries atproto)
```

`opam install atproto`, `opam pin`, and `opam install .` run `dune build -p atproto` and install the public `atproto` library. Release notes: [CHANGELOG.md](CHANGELOG.md). Official lexicons stay pinned at bluesky-social/atproto [`f0d4877a`](https://github.com/bluesky-social/atproto/commit/f0d4877a03dc8ede0d3e9a36d5b72ada63b5d2e0).

## Documentation

Browse APIs on the [odoc package page](https://david-engelmann.github.io/atproto/atproto/). This README is install, env, and examples. The Pages root (`https://david-engelmann.github.io/atproto/`) redirects there.

| Resource | Where |
| --- | --- |
| opam package | https://opam.ocaml.org/packages/atproto/ |
| API reference | https://david-engelmann.github.io/atproto/atproto/ (`dune build @doc` / `make doc`) |
| Release notes | [CHANGELOG.md](CHANGELOG.md) |
| License | [LICENSE](LICENSE) |
| Issues | https://github.com/david-engelmann/atproto/issues |
| Security | [.github/SECURITY.md](.github/SECURITY.md) |
| Contributing | [.github/CONTRIBUTING.md](.github/CONTRIBUTING.md) |
| For agents | [AGENTS.md](AGENTS.md) |

Pushes to `main` deploy odoc with GitHub Actions Pages. Pull requests also upload the `odoc-html` artifact.

## Library map

| Area | Modules | Purpose |
| --- | --- | --- |
| Session | `Auth`, `Session`, `Server` | App-password sessions, app passwords, invites, email, `getServiceAuth` |
| Identity | `Identity`, `Did_plc`, `Did_web`, `Did_key` | Handle / DID resolve, PLC directory, `did:web` / `did:key` |
| AppView | `Actor`, `Feed`, `Graph`, `Bookmark`, `Labeler`, `Unspecced` | Profiles, timelines, search, graphs, bookmarks, public labelers |
| Records | `Repo`, `Records`, `Embed`, `Facet` | create/put/delete/applyWrites and typed post/like/follow/… builders |
| Sync | `Sync`, `Repo_sync`, `Mst`, `Cid`, `Car`, `Dag_cbor` | Repo CAR, MST, CID; indexer / backfill toolkit (**not** a hosted Tap) |
| Firehose | `Firehose`, `Websocket`, `Jetstream` | `subscribeRepos` and Jetstream live tail / archive HTTP |
| OAuth | `Oauth`, `Oauth_scope` | PKCE, DPoP, public HTTPS client-metadata, browser login |
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
| Experimental | `Lt_hash`, `At_uri.Space`, `Space_commit`, `Space_credential`, `Space_xrpc`, `Space_sync` | Proposal [0016](https://github.com/bluesky-social/proposals/blob/main/0016-permissioned-data/README.md) only. **Not** a spaces product API |

Functions are in [odoc](https://david-engelmann.github.io/atproto/atproto/). What shipped when is in the [CHANGELOG](CHANGELOG.md).

## Environment

Create a `.env` (see `sample.env`) when you need a session or a non-default host.

| Variable | Purpose |
| --- | --- |
| `ATP_AUTH` | `EmailAddress:AppPassword` — use an [App Password](https://bsky.app/settings/app-passwords) |
| `ATP_HOST` | PDS / entryway host **without** a scheme (`bsky.social`; `localhost:2583` locally) |
| `ATP_SCHEME` | `https` (default) or `http` for a local stack without TLS |
| `ATP_PUBLIC` | Set `1` / `true` / `yes` / `on` to run unauthenticated public-network tests. Leave unset so `with-test` stays offline. |

Optional hosts (all without a scheme): `ATP_APPVIEW_HOST` (default `public.api.bsky.app`), `ATP_APPVIEW_DID` (default `did:web:api.bsky.app`), `ATP_OZONE_HOST` (default `localhost:2587`) / `ATP_OZONE_DID`, `ATP_CHAT_HOST` / `ATP_CHAT_DID`, `ATP_VIDEO_HOST`. Local extras: `ATP_AUTH_BOB`, `ATP_AUTH_OZONE`, `PLC_ORIGIN`, `BASE_ENDPOINT` (default `xrpc`). `sample.env` has the full list.

Session creation, repo writes, graph mutes, bookmarks, chat, ozone, and most feed helpers need `ATP_AUTH`. Chat calls need a DM-capable session: OAuth `transition:chat.bsky` or `include:chat.bsky.authFullChatClient`, or a privileged app-password. `ATP_CHAT=1` is a live-test opt-in (also implied when the session JWT already has a chat grant). Public identity (`ATP_HOST` / `bsky.social`), DID PLC, firehose subscribe, AppView reads (`public.api.bsky.app`), and most `com.atproto.sync.*` reads do **not** need auth. Those live tests skip unless `ATP_PUBLIC` is set.

Other live flags (unset in CI): `ATP_PUBLIC`, `ATP_CHAT`, `ATP_PHONE` / `ATP_PHONE_NUMBER`, `ATP_PUSH` / `ATP_PUSH_DID` / `ATP_PUSH_TOKEN` / `ATP_PUSH_APP_ID` / `ATP_PUSH_PLATFORM`, `ATP_SPACE` / `ATP_SPACE_HOST` (no default space host), `JETSTREAM_API_KEY` (alias `JETSTREAM_ARCHIVE_TOKEN`).

## Hosted Bluesky services

Clients for Bluesky-hosted chat, video, contacts, push, and Jetstream. This repo does not run those services. Browse the modules on [odoc](https://david-engelmann.github.io/atproto/atproto/). Offline examples are under `examples/`.

- **OAuth.** You host `client-metadata.json` and the redirect. The library builds the document and drives authorize → code → token. Authed AppView / Ozone / chat use `Oauth.get_service_auth`, not the DPoP access token. **DPoP cannot be proxied.** See `examples/oauth_https_metadata.ml`.
- **Chat.** `Oauth.default_scope` is not enough. Use `Oauth.default_chat_scope` (`transition:chat.bsky`) or `Oauth_scope.full_chat_client_scope`. Privileged app-passwords carry a chat grant; regular ones do not. Password path goes through the PDS with `atproto-proxy`; OAuth path calls `api.bsky.chat` with service-auth. See `examples/chat_production.ml`.
- **Video.** Service-auth audience is `did:web:<pds-host>` (`Video.pds_audience`), not `did:web:video.bsky.app`. Embed the job **blob ref**, not the HLS playlist. See `examples/video_production.ml`.
- **Indexer.** `Repo_sync` backfills and applies the firehose. It is not a Tap host. See `examples/repo_sync_indexer.ml`.
- **Jetstream.** Live `subscribe` is unauthenticated. Archive HTTP needs an operator `JETSTREAM_API_KEY` from [bsky.network/account](https://bsky.network/account). `require_archive_token` raises if the key is missing, before any HTTP. See `examples/jetstream_archive.ml`.
- **SMS / push.** Hosted Bluesky SMS (`ATP_PHONE=1` and an E.164 number you own). `Notification.register_push` takes a caller gateway. Official Bluesky push is closed to the official app. See `examples/contacts_production.ml`.

## What this package does not host

The library talks to these services. It does not run them.

- **HTTPS `client-metadata.json` and the browser redirect** — your application hosts them. The library builds the document and drives authorize → code → token.
- **Tap** — `Repo_sync` is the indexer library. This repo does not fake a Tap host.
- **Video transcoder** — talk to `video.bsky.app`. No local transcoder.
- **OSS chat** — `@atproto/dev-env` 0.6.4 does not start one (`ozone.chatUrl` = `localhost:2590`, “must run separate chat service”). Talk to `api.bsky.chat`.
- **Jetstream archive key** — the operator supplies `JETSTREAM_API_KEY`. Live subscribe stays unauthenticated.
- **SMS / phone-verification gateway** — hosted Bluesky SMS only. `requestPhoneVerification` is not faked.
- **APNs/FCM** — official Bluesky push is closed to the official app; third-party clients host their own gateway.
- **Spaces** — `Lt_hash`, `At_uri.Space`, `Space_commit`, `Space_credential`, `Space_xrpc`, and `Space_sync` implement draft proposal [0016](https://github.com/bluesky-social/proposals/blob/main/0016-permissioned-data/README.md). They are **experimental**, not a stable product API. Deferred: `com.atproto.simplespace.*`, `space:` OAuth scopes, proposal `registerNotify` `repo`. Those tests skip unless `ATP_SPACE=1` and `ATP_SPACE_HOST` names a real host. This repo does not fake a space host.

Official lexicons are pinned at [`f0d4877a`](https://github.com/bluesky-social/atproto/commit/f0d4877a03dc8ede0d3e9a36d5b72ada63b5d2e0). CI `@lexicon-coverage` fails if that pin grows and a public client NSID lacks a helper (or an explicit skip). That gate is not `dune runtest` / opam `with-test`. Hosted-only *servers* are not skip reasons.

## Development

```shell
opam install . --deps-only --with-test
dune build
dune runtest
```

`dune build` typechecks `examples/offline.ml` against the public API (no network). `dune runtest` also runs the offline examples. Release-style: `dune build -p atproto` and `dune runtest -p atproto`. Credentialed Bluesky tests skip unless `ATP_AUTH` is a real `email:app-password` pair (placeholders in `sample.env` don't count). Unauthenticated public-network tests (handle resolve, PLC, `getLatestCommit`, `subscribeRepos`, AppView reads) skip unless `ATP_PUBLIC` is set. Default GitHub TestSuite leaves `ATP_PUBLIC` unset so opam `with-test` stays offline. Re-run those tests with `make test-public` or the optional **PublicLive** workflow (`workflow_dispatch` only).

### Local AT Protocol network

`make test-pds` starts Bluesky’s official [`@atproto/dev-env@0.6.4`](https://www.npmjs.com/package/@atproto/dev-env): PLC (`:2582`), PDS (`:2583`), AppView (`:2584`), Ozone (`:2587`). Chat, video, Tap, SMS, and push are **not** in that stack.

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

Point the client at the stack with `ATP_SCHEME=http`, `ATP_HOST=localhost:2583`, `ATP_APPVIEW_HOST=localhost:2584`, `ATP_OZONE_HOST=localhost:2587`, `ATP_AUTH=alice.test:hunter2`. Mock accounts come from official `generateMockSetup` (`alice.test` / `bob.test` / ozone admin `admin-mod.test`).

`ATP_LOCAL_PDS=1` or `ATP_HOST` on localhost selects those tests (`scripts/local-atproto.sh env` exports `ATP_LOCAL_PDS=1`). If the stack is down they skip, unless `ATP_REQUIRE_LOCAL_PDS=1` (CI and `scripts/local-atproto.sh env`), in which case a down stack or failed protocol call fails the test. If the network is up, a failed protocol call **fails the test**.

OAuth against this TestNetwork (loopback metadata, PAR, DPoP, service-auth) is covered by `test/test_local_oauth.ml`. See [.github/CONTRIBUTING.md](.github/CONTRIBUTING.md).

## Examples

Examples under `examples/`. `dune build` typechecks them. None of them start a hosted service.

| File | Demo |
| --- | --- |
| [`examples/quickstart.ml`](examples/quickstart.ml) | Public network: resolve a handle (entryway) and search posts (AppView); no `ATP_AUTH` |
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
