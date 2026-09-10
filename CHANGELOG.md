# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).
Current package version is **1.0.2** (`dune-project` / `atproto.opam`).
This revision does **not** create a git tag or GitHub Release.
Tagged **1.0.1** is at
[`53ffbc2`](https://github.com/david-engelmann/atproto/releases/tag/1.0.1).
The public opam package is
[ocaml/opam-repository#30703](https://github.com/ocaml/opam-repository/pull/30703)
(to be superseded after this cut).

This file is the human-readable release history. PR numbers are
included sparingly so a change can be traced; they are not a
substitute for `git log`.

## [Unreleased]

### Added

Experimental permissioned-data / spaces helpers from proposal
[0016](https://github.com/bluesky-social/proposals/blob/main/0016-permissioned-data/README.md).
These modules landed on `main` before the 1.0.1 tag, so they are
present in that tree, but they are **not a spaces product API**.
The proposal is not final. This repo does not start or stub a
space host. Live hops skip unless `ATP_SPACE=1` and
`ATP_SPACE_HOST` names a real host.

- `Lt_hash` — 0016 commit digest (1024-lane LtHash; official empty /
  one / two snapshot vectors from bluesky-social/atproto#5187)
- `At_uri.Space` — parse/serialize
  `at://{spaceDid}/space/{type}/{skey}` and the six-segment record
  form
- `Space_commit` — deniable signed commits (`ctx`, HKDF-Expand MAC,
  ES256 / ES256K `sig`); `of_lt_hash` wires `Lt_hash.hash`
- `Space_credential` — delegation / space-credential /
  client-attestation JWTs plus DPoP helpers (reuses `Oauth`; no
  DPoP nonce)
- `Space_xrpc` — draft `com.atproto.space.*` query/JSON builders
  and `Client` wrappers (credential exchange, permissioned repo
  read/write, sync queries, notify)
- `Space_sync` — offline `listRepoOps` apply and two-root CAR
  encode/apply (signed commit, then DRISL index, then records)

Deferred: `com.atproto.simplespace.*`, `space:` OAuth scopes, and
proposal `registerNotify` `repo` (not in the #5187 lexicon).

### Changed

- Public docs rewritten for third-party readers (README product
  pitch and module map, 1.0.1 notes now that the tag exists, warmer
  odoc landing). Addresses the readability concern on
  ocaml/opam-repository#30695

### Notes

- No lexicon pin bump. Official lexicons stay bluesky-social/atproto
  [`f0d4877a`](https://github.com/bluesky-social/atproto/commit/f0d4877a03dc8ede0d3e9a36d5b72ada63b5d2e0).
- Hosted-only products stay listed, not faked (see the 1.0.0 Notes
  below).

## [1.0.2] - 2026-09-10

Packaging / opam-health cut so
[ocaml/opam-repository#30703](https://github.com/ocaml/opam-repository/pull/30703)
can be superseded. `atproto.1.0.1` built, linted, and passed
lower-bounds; almost every opam-ci `tests` job failed on
public-network live hops (`identity:test_resolve_actor_live`,
`did_plc:test_resolve_live` / `test_live_chain_structure`,
`sync:test_list_blobs_public` / `test_get_record_proof_public`,
`temp:test_check_handle_live`, `suite:test_search_starter_packs_*_live`,
`unspecced:test_popular_live`, and the same class of AppView /
firehose / Jetstream / did:web / HTTP/2 smokes). Those tests hit
plc.directory / AppView / a public PDS and only `skip_if` on a
caught exception. Partial success + assert, or a non-skip path,
still failed the suite. Opam `with-test` must not depend on those
hosts.

This revision sets `dune-project` / `atproto.opam` to **1.0.2**. It
does **not** create a git tag or GitHub Release and does **not**
touch ocaml/opam-repository. Tagging and superseding #30703 remain
maintainer follow-ups.

No lexicon pin bump (official pin stays `f0d4877a`). Jane Street /
OCaml bounds are unchanged. No fake hosts.

### Added

- `ATP_PUBLIC` opt-in for unauthenticated public-internet live hops
  (`Auth.public_live_enabled`, shared `Public_live.skip_unless_public`).
  Truthy values: `1` / `true` / `yes` / `on`. Default unset skips
  those tests at the start. Local TestNetwork stays on
  `ATP_LOCAL_PDS`. Credential hops stay on `ATP_AUTH`. Hosted-only
  products stay on `ATP_SPACE` / `ATP_PHONE` / `ATP_PUSH` / `ATP_CHAT`

### Changed

- Package version `1.0.1` → `1.0.2` (`dune-project` /
  `atproto.opam`)
- `dune build -p atproto @runtest` / opam `with-test` is offline-safe
  when `ATP_PUBLIC` is unset
- GitHub Actions default TestSuite `build` does **not** set
  `ATP_PUBLIC` (deterministic; no public hops). The previous default
  suite ran those hops and skipped only on request failure. Re-run
  them with `ATP_PUBLIC=1` locally (`make test-public`) or the
  optional **PublicLive** workflow (`workflow_dispatch` only; not a
  merge-when-green required check)

### Notes

- This does not touch ocaml/opam-repository
- Experimental 0016 helpers stay under [Unreleased](#unreleased)

## [1.0.1] - 2026-09-10

Released as tag
[`1.0.1`](https://github.com/david-engelmann/atproto/releases/tag/1.0.1)
(`53ffbc2` / [#243](https://github.com/david-engelmann/atproto/pull/243)).
Public opam publish:
[ocaml/opam-repository#30703](https://github.com/ocaml/opam-repository/pull/30703)
(open; supersedes #30698).

Packaging fix so opam-ci can install and test the package. The
1.0.0 submission built on 4.14 / 5.2 but failed on the lexicon
coverage gate under `with-test`, unbounded `digestif` /
`mirage-crypto-ec` lower bounds, and a redundant `version:` field
in the submitted opam file. OCaml 5.4+ remains out of range
(Jane Street v0.17).

No lexicon pin bump (official pin stays `f0d4877a`). Jane Street /
OCaml bounds are unchanged. Experimental 0016 helpers are
documented under [Unreleased](#unreleased) — present on this tree,
not a product API.

### Changed

- Package version `1.0.0` → `1.0.1` (`dune-project` /
  `atproto.opam`)
- `digestif` lower bound `>= 1.1.2` (`Digestif.SHA256` /
  `digest_string` / `hmac_string` / `to_raw_string`; unbounded
  digestif can solve to 0.5-era and fail with `Unbound module
  Digestif`)
- `mirage-crypto-ec` and `mirage-crypto-rng` lower bounds `>= 1.2.0`.
  `P256.Dsa.pub_of_octets` is the 1.x octets API (1.0.0+; 0.11 used
  `of_cstruct`). This repo also calls
  `Mirage_crypto_rng_unix.use_default` (added in 1.2.0). The 1.x
  mirage-crypto packages pin each other, so 1.2.0 is the coherent
  floor
- Official NSID coverage (`test_lexicon_coverage`) moves off
  `@runtest` to `@lexicon-coverage`. `dune build -p atproto
  @runtest` / opam `with-test` run unit tests only. TestSuite still
  runs the gate (`dune build @lexicon-coverage`)

### Notes

- opam-repository submissions should **omit** the redundant
  `version:` field (`opam lint`). Keep `(version ...)` in
  `dune-project`; the in-repo generated `atproto.opam` may still
  carry `version:`
- No `x-ci-accept-failures`. The coverage gate is not deleted
- Experimental 0016 helpers stay under [Unreleased](#unreleased)

## [1.0.0] - 2026-09-09

First stable packaged surface, tagged at
[`1.0.0`](https://github.com/david-engelmann/atproto/releases/tag/1.0.0)
(`8a1b866` / [#237](https://github.com/david-engelmann/atproto/pull/237)).
What is new since tagged **0.1.0**
(`8f44fb9` / [#228](https://github.com/david-engelmann/atproto/pull/228)).
The first 1.0.0 opam-repository submission was
[#30698](https://github.com/ocaml/opam-repository/pull/30698)
(superseded by [1.0.1](#101---2026-09-10) / #30703). The 0.1.0
opam-repository PR
[#30695](https://github.com/ocaml/opam-repository/pull/30695) is
separate.

Requires OCaml `>= 4.14.1` and `< 5.4` (CI `build`: 4.14.1 and 5.3.0).
Jane Street `core` / `async` / `ppx_jane` / `zstandard` are
`>= v0.16.0` and `< v0.18~` (v0.16 on 4.14, v0.17 on 5.1–5.3).
ocamlformat stays **0.25.1**. Official lexicon pin stays `f0d4877a`
(no bump in [#229](https://github.com/david-engelmann/atproto/pull/229)–[#236](https://github.com/david-engelmann/atproto/pull/236)).
System libzstd is required for Jetstream dict-zstd (Ubuntu/Debian
`libzstd-dev`, Homebrew `zstd`).

A GitHub pin is the 1.0.0 development surface:

```shell
opam pin add atproto git+https://github.com/david-engelmann/atproto.git
```

### Added

#### OCaml 5 / packaging

- OCaml 5 / packaging modernization
  ([#229](https://github.com/david-engelmann/atproto/pull/229)): dune
  lang 3.11, OCaml `>= 4.14.1` and `< 5.4`, Jane Street
  `>= v0.16.0` and `< v0.18~`, CI `build` on 4.14.1 + 5.3.0.
  ocamlformat stays **0.25.1**. Public Jane Street v0.17 does not
  support OCaml 5.4+; 5.0 is untested (v0.17 needs 5.1+)

#### HTTPS OAuth

- Public HTTPS client-metadata + production browser-login path
  ([#230](https://github.com/david-engelmann/atproto/pull/230)):
  `https_client_id` / `public_https_metadata` /
  `validate_https_metadata` / `metadata_document` /
  `metadata_http_response` / `fetch_client_metadata` /
  `start_browser_login` / `complete_browser_login`.
  `examples/oauth_https_metadata.ml` is offline scaffolding.
  **The application still hosts the HTTPS document and redirect URI.**
  This library does not host a login UI

#### Chat (hosted `chat.bsky.*`)

- Hosted Bluesky chat client
  ([#231](https://github.com/david-engelmann/atproto/pull/231)):
  `Oauth.default_chat_scope` / `Oauth_scope.has_chat` /
  `Chat.service_aud` / `list_convos_body` / `list_convos_service` /
  `get_messages_service` / `send_message_service` on `api.bsky.chat`.
  `examples/chat_production.ml` is offline wiring.
  **No OSS chat backend** is started or stubbed

#### Video (hosted `app.bsky.video.*`)

- Hosted Bluesky video client
  ([#232](https://github.com/david-engelmann/atproto/pull/232)):
  `Video.pds_audience` from `#atproto_pds` /
  `upload_service_auth_body` / `mint_upload_token` /
  `get_upload_limits_service` / `get_job_status_body` /
  `upload_video_body` / `part_slice` / `embed_of_blob` /
  `embed_of_job`. `examples/video_production.ml` is offline wiring.
  **No hosted transcoder** is started or stubbed

#### Repo_sync / TAP-like indexer

- Library-ready indexer / backfill path
  ([#233](https://github.com/david-engelmann/atproto/pull/233)):
  `Repo_sync.export_record_proof` / `export_record_proof_bytes` /
  `walk_json` / `record_json` / `status_to_string`,
  `Dag_cbor.to_yojson`. `examples/repo_sync_indexer.ml` is an
  offline fixture sketch. **Not a hosted Tap**

#### Jetstream archive token

- Archive HTTP operator-token env
  ([#234](https://github.com/david-engelmann/atproto/pull/234)):
  `JETSTREAM_API_KEY` / `JETSTREAM_ARCHIVE_TOKEN` / `~token` →
  `Authorization: Bearer`; `require_archive_token` /
  `~require_token`. `examples/jetstream_archive.ml` is offline
  wiring. **The operator must supply the key.** This library does
  not invent one

#### Phone / contacts / push

- Hosted phone / contacts / push client
  ([#235](https://github.com/david-engelmann/atproto/pull/235)):
  `Contact.get_matches_body` / `get_matches_appview` / `*_service`,
  `Notification.register_push_body` / `unregister_push_body` /
  `platform_ios` / `effective_push_proxy` / `Xrpc.notif_proxy`,
  `ATP_PHONE` / `ATP_PUSH` skip gates.
  `examples/contacts_production.ml` is offline wiring.
  **No SMS gateway and no APNs/FCM.**
  `requestPhoneVerification` is not faked

#### Docs

- Human-readable Keep a Changelog history
  ([#236](https://github.com/david-engelmann/atproto/pull/236)):
  `## [0.1.0]` thematic sections for the tagged RC. This **1.0.0**
  section is the packaged surface since that tag

### Changed

- Package version `0.1.0` → `1.0.0` (`dune-project` /
  `atproto.opam`)
- OCaml upper bound lifted from `< 5.0` to `< 5.4`
  ([#229](https://github.com/david-engelmann/atproto/pull/229))

### Fixed

- CI `Install libzstd` drops every GitHub-runner apt source that
  points at `dl.google.com`, so a stale Packages hash cannot fail
  required jobs
  ([#230](https://github.com/david-engelmann/atproto/pull/230))

### Notes

Honesty constraints for this 1.0.0 surface:

- **No fake OSS chat backend**, video transcoder, Tap host, SMS
  gateway, or APNs/FCM push backend
- **The app still hosts** HTTPS `client-metadata.json` (that URL is
  `client_id`) and receives the browser redirect
- **Jetstream archive HTTP** on Bluesky-hosted instances needs an
  operator API key (`JETSTREAM_API_KEY` / `JETSTREAM_ARCHIVE_TOKEN`,
  or `~token`). The library does not invent one
- **No lexicon pin bump** in the #229–#236 series (pin stays
  `f0d4877a`). Newly published official lexicons after that SHA
  fail the coverage gate until the snapshot and bindings (or an
  explicit skip) are updated
- Hosted-only SMS / APNs-FCM / unhosted feed generator stay listed
  not faked (`requestPhoneVerification` is not faked)

### Not in this release

- An application still has to host the HTTPS `client-metadata.json`
  and receive the browser redirect (this library builds/validates
  the document and drives authorize → code → token; it does not
  host them or a login UI)
- Hosted Tap service (the indexer / backfill library path is
  documented and library-ready; this package still does not fake
  a Tap host)
- Hosted video transcoder (the production client path is documented
  and library-ready; this package still does not fake a local
  transcoder)
- Official OSS chat backend (TestNetwork does not start one). The
  production hosted path is documented and library-ready; this
  package still does not fake a local chat service
- Newly published official lexicons after bluesky-social/atproto
  `f0d4877a` — the coverage gate fails until the pin snapshot and
  bindings (or an explicit skip) are updated
- Jetstream archive HTTP download on Bluesky-hosted instances still
  requires the operator to supply an API key
- Hosted SMS / phone-verification gateway (the production client
  path is documented and library-ready; this package still does
  not fake `requestPhoneVerification` or a local SMS service)
- Official OSS push gateway (official Bluesky push is closed to
  the official app; the production client path is documented and
  library-ready; this package still does not fake APNs/FCM)
- Permissioned data / spaces / LtHash (no stable public spec yet)

## [0.1.0] - 2026-09-09

First public opam release candidate, tagged at
[#228](https://github.com/david-engelmann/atproto/pull/228)
(`8f44fb9`). The packaged GitHub surface is now [1.0.0](#100---2026-09-09).

The 0.1.0 opam-repository PR
[#30695](https://github.com/ocaml/opam-repository/pull/30695) was
superseded by 1.0.0 / 1.0.1. A GitHub pin still works for
development:

```shell
opam pin add atproto git+https://github.com/david-engelmann/atproto.git
```

Requires OCaml `>= 4.14.1` and `< 5.4` (CI `build`: 4.14.1 and 5.3.0).
Jane Street `core` / `async` / `ppx_jane` / `zstandard` are
`>= v0.16.0` and `< v0.18~` (v0.16 on 4.14, v0.17 on 5.1–5.3). System
libzstd is required for Jetstream dict-zstd (Ubuntu/Debian
`libzstd-dev`, Homebrew `zstd`).

The first installable cut was 2026-09-03. Work in
[#229](https://github.com/david-engelmann/atproto/pull/229)–[#236](https://github.com/david-engelmann/atproto/pull/236)
shipped on main after the 0.1.0 tag while the package version was
still 0.1.0; that wave is packaged as [1.0.0](#100---2026-09-09).
The sections below remain the human-readable 0.1.0-era history
(through [#235](https://github.com/david-engelmann/atproto/pull/235)).

### Added

#### Protocol client

- XRPC GET/POST (Cohttp) plus HTTP/2 TLS (`Http_client`) for public HTTPS
- Session / JWT (`Auth`, `Session`), including `authFactorToken` /
  `allowTakendown` and typed `getSession`
- AppView: actor, feed, graph, bookmark, notification, labeler, unspecced,
  drafts, contacts, age assurance
- Repo writes and typed record builders (`Repo`, `Records`)
- Identity, DID PLC/web/key, CID/CAR/DAG-CBOR, MST, TID, AT URI, firehose
- Lexicon 1 parse / validate / `to_ocaml`, including bundled official
  documents (pin `f0d4877a`)
- `site.standard.*` and `com.germnetwork.declaration` record builders
- `com.atproto.admin` and `com.atproto.temp` clients (no invented
  operator session)
- Local TestNetwork via published `@atproto/dev-env@0.6.4`
  (`make test-pds` / `ATP_REQUIRE_LOCAL_PDS=1`)

#### OAuth / DPoP

- PKCE S256, DPoP ES256 + nonce, PAR, token, refresh, RFC 7009 revoke,
  granular scopes, official `app.bsky.auth*` /
  `chat.bsky.authFullChatClient` permission-sets
- Public HTTPS client-metadata + production browser-login path
  ([#230](https://github.com/david-engelmann/atproto/pull/230)):
  `https_client_id` / `public_https_metadata` /
  `validate_https_metadata` / `metadata_document` /
  `metadata_http_response` / `fetch_client_metadata` /
  `start_browser_login` / `complete_browser_login`.
  `examples/oauth_https_metadata.ml` is offline scaffolding.
  **The application still hosts the HTTPS document and redirect URI.**
  This library does not host a login UI.
- Local TestNetwork loopback metadata, AS discovery, PAR, authorize GET,
  `~api/sign-in` / `~api/consent` with real cookies, DPoP `getSession`,
  AppView / Ozone `getServiceAuth` (DPoP cannot be proxied)
- Chat-capable scopes
  ([#231](https://github.com/david-engelmann/atproto/pull/231)):
  `Oauth.default_chat_scope` / `Oauth_scope.has_chat` /
  `full_chat_client_scope`. `Oauth.default_scope` is not enough for DMs.

#### Chat (hosted `chat.bsky.*`)

- Hosted Bluesky chat client
  ([#231](https://github.com/david-engelmann/atproto/pull/231)):
  `Chat.service_aud` / `default_host` / `ATP_CHAT_HOST`,
  query-pair helpers shared with service-auth `list_convos_service` /
  `get_convo_service` / `get_messages_service` / `send_message_service`
  on `api.bsky.chat`. Password sessions still send `atproto-proxy`.
  `examples/chat_production.ml` is offline wiring.
- **No OSS chat backend** is started or stubbed. Official TestNetwork
  0.6.4 still has none (`ozone.chatUrl` = `localhost:2590`, “must run
  separate chat service”). Live DM tests stay skippable unless
  `ATP_AUTH` has a chat/DM scope or `ATP_CHAT=1`.

#### Video (hosted `app.bsky.video.*`)

- Hosted Bluesky video client
  ([#232](https://github.com/david-engelmann/atproto/pull/232)):
  `Video.pds_audience` (session `#atproto_pds`, not
  `did:web:video.bsky.app`), `upload_service_auth_body` /
  `mint_upload_token` (`lxm` = `com.atproto.repo.uploadBlob`),
  `get_upload_limits_service`, job / multipart helpers (`part_slice`),
  `embed_of_blob` / `embed_of_job` (create embed is the blob ref, not
  the playlist). `examples/video_production.ml` is offline wiring.
- **No hosted transcoder** is started or stubbed. Official TestNetwork
  still has none. Live `getUploadLimits` stays skippable unless
  `ATP_AUTH` is a real credential.

#### Repo_sync / TAP-like indexer

- Library-ready indexer / backfill path
  ([#233](https://github.com/david-engelmann/atproto/pull/233)):
  `Repo_sync.export_record_proof` / `export_record_proof_bytes` /
  `walk_json` / `record_json` / `status_to_string`,
  `Dag_cbor.to_yojson`, firehose apply, `#sync` desync, Sync 1.1
  export, offline `write_signed_repo`.
  `examples/repo_sync_indexer.ml` is an offline fixture sketch.
- **Not a hosted Tap.** Official TestNetwork is a local PDS + AppView +
  Ozone stack, not a Tap host.

#### Jetstream

- v2 live tail, collection/DID/kind filters, seq + unix-µs cursors,
  reconnect/dedupe, v1 `/subscribe` compat
- Live dict-zstd `subscribeEvents`
  ([#109](https://github.com/david-engelmann/atproto/pull/109)): Jane
  Street `zstandard`, `getZstdDictionary`, v2 `zstdDictionary=<id>`
- v2 `subscribe` / `subscribe_one` offer
  `Sec-WebSocket-Protocol: xrpc.v1.json`
  ([#110](https://github.com/david-engelmann/atproto/pull/110)); RFC
  6455 §4.1 echo required. Unoffered connections unchanged. v2 stays
  server-push only
- Archive HTTP operator-token env
  ([#234](https://github.com/david-engelmann/atproto/pull/234)):
  `JETSTREAM_API_KEY` / `JETSTREAM_ARCHIVE_TOKEN` / `~token` →
  `Authorization: Bearer`; `require_archive_token` /
  `~require_token`. `examples/jetstream_archive.ml` is offline wiring.
  **The operator must supply the key.** This library does not invent
  one. Live `subscribeEvents` stays unauthenticated. Self-hosted
  archives may omit the key.

#### Phone / contacts / push (hosted AppView)

- Hosted phone / contacts / push client
  ([#235](https://github.com/david-engelmann/atproto/pull/235)):
  `Contact.get_matches_body` / `get_matches_appview` / `*_service`,
  `Notification.register_push_body` / `unregister_push_body` /
  `platform_ios` / `effective_push_proxy` / `Xrpc.notif_proxy`,
  `ATP_PHONE` / `ATP_PUSH` skip gates.
  `examples/contacts_production.ml` is offline wiring.
- **No SMS gateway and no APNs/FCM.** Official TestNetwork still has
  no phone or push service. `requestPhoneVerification` is not faked.
  Official Bluesky push is closed to the official app; third-party
  clients host their own gateway.

#### Ozone

- Typed encodings: `emit_event` / `emit_event_service_typed`
  ([#187](https://github.com/david-engelmann/atproto/pull/187),
  [#188](https://github.com/david-engelmann/atproto/pull/188)),
  `get_account_preferences` via `Actor.preferences`
  ([#186](https://github.com/david-engelmann/atproto/pull/186)),
  `create_activity`
  ([#191](https://github.com/david-engelmann/atproto/pull/191)),
  `schedule_action`
  ([#196](https://github.com/david-engelmann/atproto/pull/196)). Raw
  Yojson siblings stay unchanged.
- Yojson body / query-pair helpers for scheduled actions, communication
  templates, safelink, report, set/setting/team, queue, `query_events` /
  `query_statuses`, leftover `query_reports` / `search_repos`
  ([#197](https://github.com/david-engelmann/atproto/pull/197),
  [#211](https://github.com/david-engelmann/atproto/pull/211)–[#216](https://github.com/david-engelmann/atproto/pull/216),
  [#219](https://github.com/david-engelmann/atproto/pull/219),
  [#225](https://github.com/david-engelmann/atproto/pull/225)).
  Queue assign/unassign/route only — not `report.assignModerator`.
  Does not invent leftover unused `managerRole` / `lang`.
- Live TestNetwork hops for communication templates / sets
  ([#119](https://github.com/david-engelmann/atproto/pull/119)), leftover
  queue / report / setting NSIDs
  ([#123](https://github.com/david-engelmann/atproto/pull/123),
  [#129](https://github.com/david-engelmann/atproto/pull/129),
  [#154](https://github.com/david-engelmann/atproto/pull/154)),
  `getAccountPreferences`
  ([#137](https://github.com/david-engelmann/atproto/pull/137)). Skip
  when the NSID is not served or TestNetwork policy applies. Does not
  fake a hosted ozone store.

#### Client XRPC migration and leftover helpers

JSON XRPC that still hand-rolled Cohttp now shares `Client.get_json` /
`Client.get_text` / `Client.post_json` and query-pair or POST body
helpers. Public signatures and parse types stay unchanged unless noted.
Binary `upload_blob` / `import_repo` and Sync CAR stay Cohttp.

- Repo create/put/delete Yojson helpers and `blob_ref_to_json`
  ([#192](https://github.com/david-engelmann/atproto/pull/192)–[#194](https://github.com/david-engelmann/atproto/pull/194))
- Moderation `create_report` Yojson bodies
  ([#195](https://github.com/david-engelmann/atproto/pull/195))
- Graph mute/unmute Client + list/thread mute bodies
  ([#198](https://github.com/david-engelmann/atproto/pull/198),
  [#209](https://github.com/david-engelmann/atproto/pull/209))
- Label `queryLabels`, Actor profile/search/suggestions
  ([#199](https://github.com/david-engelmann/atproto/pull/199),
  [#200](https://github.com/david-engelmann/atproto/pull/200))
- Repo / Feed / Notification / Server / Session / Identity / Sync /
  `getServiceAuth` leftover JSON via Client
  ([#201](https://github.com/david-engelmann/atproto/pull/201)–[#208](https://github.com/david-engelmann/atproto/pull/208))
- Notification `putPreferences` v1 + `putActivitySubscription` bodies
  ([#210](https://github.com/david-engelmann/atproto/pull/210)); leftover
  `list_notifications` query pairs
  ([#226](https://github.com/david-engelmann/atproto/pull/226))
- Label `label_value_definition` encode + Labeler `policies_to_json`
  ([#217](https://github.com/david-engelmann/atproto/pull/217))
- Site typed record encodes
  ([#218](https://github.com/david-engelmann/atproto/pull/218))
- Typed Actor `putPreferences` and Draft create/update
  ([#185](https://github.com/david-engelmann/atproto/pull/185),
  [#190](https://github.com/david-engelmann/atproto/pull/190))
- Xrpc `x-atproto-bsky-topics` helpers
  ([#189](https://github.com/david-engelmann/atproto/pull/189))
- Feed / Graph / Unspecced leftover AppView query-pair helpers
  ([#220](https://github.com/david-engelmann/atproto/pull/220)–[#224](https://github.com/david-engelmann/atproto/pull/224),
  [#227](https://github.com/david-engelmann/atproto/pull/227)):
  currently sent fields only; no leftover unused lexicon fields
  invented

#### Local TestNetwork coverage

Live hops against `@atproto/dev-env@0.6.4`. Skip only when the NSID is
not served or TestNetwork policy applies (email-token, UpstreamFailure,
feature-disabled). Throwaway accounts never use `alice.test` for
destructive calls. Hosted chat / video / Tap / SMS / push stay listed
not faked.

- PDS handle update, activate/deactivate, AppView drafts / list feed /
  mute list / notification prefs
  ([#116](https://github.com/david-engelmann/atproto/pull/116))
- Leftover AppView / PDS / ozone NSIDs after that
  ([#123](https://github.com/david-engelmann/atproto/pull/123),
  [#129](https://github.com/david-engelmann/atproto/pull/129))
- Remaining `com.atproto.admin` / `com.atproto.server` /
  `com.atproto.temp` operator NSIDs
  ([#150](https://github.com/david-engelmann/atproto/pull/150),
  [#152](https://github.com/david-engelmann/atproto/pull/152),
  [#162](https://github.com/david-engelmann/atproto/pull/162))
- AppView `sendInteractions` / `describeFeedGenerator` /
  `putPreferences` v1, leftover `getFeedSkeleton`, unspecced
  age-assurance
  ([#153](https://github.com/david-engelmann/atproto/pull/153),
  [#163](https://github.com/david-engelmann/atproto/pull/163),
  [#168](https://github.com/david-engelmann/atproto/pull/168))
- `interestsPref.updatedAt` get/put preferences hops
  ([#184](https://github.com/david-engelmann/atproto/pull/184);
  parse shipped in [#181](https://github.com/david-engelmann/atproto/pull/181))

#### Docs, CI, and packaging

- odoc HTML on GitHub Pages
  ([#112](https://github.com/david-engelmann/atproto/pull/112),
  [#113](https://github.com/david-engelmann/atproto/pull/113)):
  https://david-engelmann.github.io/atproto/
- Function-level odoc and landing-map sweep across public modules
  ([#114](https://github.com/david-engelmann/atproto/pull/114),
  [#117](https://github.com/david-engelmann/atproto/pull/117),
  [#122](https://github.com/david-engelmann/atproto/pull/122),
  [#125](https://github.com/david-engelmann/atproto/pull/125)–[#126](https://github.com/david-engelmann/atproto/pull/126),
  [#128](https://github.com/david-engelmann/atproto/pull/128),
  [#130](https://github.com/david-engelmann/atproto/pull/130)–[#136](https://github.com/david-engelmann/atproto/pull/136),
  [#138](https://github.com/david-engelmann/atproto/pull/138),
  [#140](https://github.com/david-engelmann/atproto/pull/140)–[#149](https://github.com/david-engelmann/atproto/pull/149),
  [#151](https://github.com/david-engelmann/atproto/pull/151),
  [#156](https://github.com/david-engelmann/atproto/pull/156)–[#161](https://github.com/david-engelmann/atproto/pull/161),
  [#165](https://github.com/david-engelmann/atproto/pull/165)–[#166](https://github.com/david-engelmann/atproto/pull/166),
  [#170](https://github.com/david-engelmann/atproto/pull/170),
  [#177](https://github.com/david-engelmann/atproto/pull/177))
- Compiled-only `examples/offline.ml` constructor/parser coverage
  ([#171](https://github.com/david-engelmann/atproto/pull/171))
- Installed-package consumer smoke
  ([#172](https://github.com/david-engelmann/atproto/pull/172)) — not
  an opam-repository publish
- libzstd install notes
  ([#174](https://github.com/david-engelmann/atproto/pull/174),
  [#176](https://github.com/david-engelmann/atproto/pull/176))
- merge-when-green squash automation
  ([#179](https://github.com/david-engelmann/atproto/pull/179));
  lexicon-pin drift is advisory and must not deadlock merges
  ([#182](https://github.com/david-engelmann/atproto/pull/182))
- Lexicon pin `f0d4877a` for actor interests `updatedAt`
  ([#132](https://github.com/david-engelmann/atproto/pull/132),
  [#181](https://github.com/david-engelmann/atproto/pull/181))
- Docs-only notes hygiene PRs that previously restated this file
  ([#121](https://github.com/david-engelmann/atproto/pull/121),
  [#127](https://github.com/david-engelmann/atproto/pull/127),
  [#139](https://github.com/david-engelmann/atproto/pull/139),
  [#155](https://github.com/david-engelmann/atproto/pull/155),
  [#164](https://github.com/david-engelmann/atproto/pull/164),
  [#167](https://github.com/david-engelmann/atproto/pull/167),
  [#169](https://github.com/david-engelmann/atproto/pull/169),
  [#173](https://github.com/david-engelmann/atproto/pull/173),
  [#175](https://github.com/david-engelmann/atproto/pull/175),
  [#178](https://github.com/david-engelmann/atproto/pull/178),
  [#180](https://github.com/david-engelmann/atproto/pull/180),
  [#183](https://github.com/david-engelmann/atproto/pull/183))
- opam-repository publish prep
  ([#228](https://github.com/david-engelmann/atproto/pull/228)):
  package description / README / CONTRIBUTING / odoc landing say the
  package is published on opam-repository
- OCaml 5 / packaging modernization hop toward 1.0.0
  ([#229](https://github.com/david-engelmann/atproto/pull/229)): dune
  lang 3.11, OCaml `>= 4.14.1` and `< 5.4`, Jane Street
  `>= v0.16.0` and `< v0.18~`, CI `build` on 4.14.1 + 5.3.0.
  ocamlformat stays **0.25.1**. Package version stays `0.1.0`

### Changed

- OCaml upper bound lifted from `< 5.0` to `< 5.4`
  ([#229](https://github.com/david-engelmann/atproto/pull/229)). Public
  Jane Street v0.17 does not support OCaml 5.4+; 5.0 is untested
  (v0.17 needs 5.1+)
- Leftover JSON XRPC callers share `Client` helpers and `*_body`
  query/POST builders instead of hand-rolled Cohttp
  ([#192](https://github.com/david-engelmann/atproto/pull/192)–[#227](https://github.com/david-engelmann/atproto/pull/227)).
  Empty procedure output stays `""`. String create/put/delete record
  APIs stay
- `Ozone.get_account_preferences` preferences are
  `Actor.preference list` (compile-breaking; no backwards-compat)
  ([#186](https://github.com/david-engelmann/atproto/pull/186))

### Fixed

- CI `Install libzstd` drops every GitHub-runner apt source that
  points at `dl.google.com`, so a stale Packages hash cannot fail
  required jobs
  ([#230](https://github.com/david-engelmann/atproto/pull/230))
- `Identity.resolve_did` / `resolve_identity` fall back to PLC /
  `did:web` when the host returns `MethodNotImplemented` (PDS 0.5.x
  and current entryway)

### Notes

Honesty constraints for this 0.1.0 surface:

- **No fake OSS chat backend**, video transcoder, Tap host, SMS
  gateway, or APNs/FCM push backend
- **The app still hosts** HTTPS `client-metadata.json` (that URL is
  `client_id`) and receives the browser redirect
- **Jetstream archive HTTP** on Bluesky-hosted instances needs an
  operator API key (`JETSTREAM_API_KEY` / `JETSTREAM_ARCHIVE_TOKEN`,
  or `~token`). The library does not invent one
- **No lexicon pin bump** in the #228–#235 production-path series
  (pin stays `f0d4877a`). Newly published official lexicons after
  that SHA fail the coverage gate until the snapshot and bindings
  (or an explicit skip) are updated
- Hosted-only SMS / APNs-FCM / unhosted feed generator stay listed
  not faked (`requestPhoneVerification` is not faked)

`examples/offline.ml` typechecks against the public API under
`dune build` / `dune runtest`.

#### PR index (#112–#235)

Grouped so the old “on top of #N” sentence is recoverable without
rereading it:

| Theme | Pull requests |
| --- | --- |
| Docs / Pages / odoc / CI | [#112](https://github.com/david-engelmann/atproto/pull/112)–[#114](https://github.com/david-engelmann/atproto/pull/114), [#117](https://github.com/david-engelmann/atproto/pull/117), [#120](https://github.com/david-engelmann/atproto/pull/120)–[#122](https://github.com/david-engelmann/atproto/pull/122), [#125](https://github.com/david-engelmann/atproto/pull/125)–[#128](https://github.com/david-engelmann/atproto/pull/128), [#130](https://github.com/david-engelmann/atproto/pull/130)–[#136](https://github.com/david-engelmann/atproto/pull/136), [#138](https://github.com/david-engelmann/atproto/pull/138)–[#149](https://github.com/david-engelmann/atproto/pull/149), [#151](https://github.com/david-engelmann/atproto/pull/151), [#155](https://github.com/david-engelmann/atproto/pull/155)–[#161](https://github.com/david-engelmann/atproto/pull/161), [#164](https://github.com/david-engelmann/atproto/pull/164)–[#183](https://github.com/david-engelmann/atproto/pull/183) |
| Live TestNetwork leftovers | [#116](https://github.com/david-engelmann/atproto/pull/116), [#119](https://github.com/david-engelmann/atproto/pull/119), [#123](https://github.com/david-engelmann/atproto/pull/123), [#129](https://github.com/david-engelmann/atproto/pull/129), [#137](https://github.com/david-engelmann/atproto/pull/137), [#150](https://github.com/david-engelmann/atproto/pull/150), [#152](https://github.com/david-engelmann/atproto/pull/152)–[#154](https://github.com/david-engelmann/atproto/pull/154), [#162](https://github.com/david-engelmann/atproto/pull/162)–[#163](https://github.com/david-engelmann/atproto/pull/163), [#168](https://github.com/david-engelmann/atproto/pull/168), [#184](https://github.com/david-engelmann/atproto/pull/184) |
| Lexicon pin `f0d4877a` | [#132](https://github.com/david-engelmann/atproto/pull/132), [#181](https://github.com/david-engelmann/atproto/pull/181)–[#182](https://github.com/david-engelmann/atproto/pull/182) |
| Typed encodings / leftover Client helpers | [#185](https://github.com/david-engelmann/atproto/pull/185)–[#227](https://github.com/david-engelmann/atproto/pull/227) |
| Packaging / OCaml 5 | [#228](https://github.com/david-engelmann/atproto/pull/228)–[#229](https://github.com/david-engelmann/atproto/pull/229) |
| OAuth HTTPS metadata | [#230](https://github.com/david-engelmann/atproto/pull/230) |
| Hosted chat | [#231](https://github.com/david-engelmann/atproto/pull/231) |
| Hosted video | [#232](https://github.com/david-engelmann/atproto/pull/232) |
| Repo_sync / TAP-like indexer | [#233](https://github.com/david-engelmann/atproto/pull/233) |
| Jetstream archive token | [#234](https://github.com/david-engelmann/atproto/pull/234) |
| Phone / contacts / push | [#235](https://github.com/david-engelmann/atproto/pull/235) |

Earlier Jetstream work that shipped in the 2026-09-03 cut:
[#109](https://github.com/david-engelmann/atproto/pull/109)–[#110](https://github.com/david-engelmann/atproto/pull/110).

### Not in this release

- An application still has to host the HTTPS `client-metadata.json`
  and receive the browser redirect (this library builds/validates
  the document and drives authorize → code → token; it does not
  host them or a login UI)
- Hosted Tap service (the indexer / backfill library path is
  documented and library-ready; this package still does not fake
  a Tap host)
- Hosted video transcoder (the production client path is documented
  and library-ready; this package still does not fake a local
  transcoder)
- Official OSS chat backend (TestNetwork does not start one). The
  production hosted path is documented and library-ready; this
  package still does not fake a local chat service
- Newly published official lexicons after bluesky-social/atproto
  `f0d4877a` — the coverage gate fails until the pin snapshot and
  bindings (or an explicit skip) are updated
- Jetstream archive HTTP download on Bluesky-hosted instances still
  requires the operator to supply an API key. Live compressed
  `subscribeEvents` and `xrpc.v1.json` are implemented. Self-hosted
  archives may omit the key
- Hosted SMS / phone-verification gateway (the production client
  path is documented and library-ready; this package still does
  not fake `requestPhoneVerification` or a local SMS service)
- Official OSS push gateway (official Bluesky push is closed to
  the official app; the production client path is documented and
  library-ready; this package still does not fake APNs/FCM)
- Permissioned data / spaces / LtHash (no stable public spec yet)
