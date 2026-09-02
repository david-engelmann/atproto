# Changelog

Notes for the packaged **0.1.0** library. This file records what actually
shipped through [#121](https://github.com/david-engelmann/atproto/pull/121)
(CHANGELOG through #120 + `sample.env` leftover cleanup), including
[#120](https://github.com/david-engelmann/atproto/pull/120)
(github-actions group bump),
[#119](https://github.com/david-engelmann/atproto/pull/119) (README Quick
start + `make doc` + live ozone `createTemplate` / `upsertSet` with
`createdBy` / `updatedBy`),
[#117](https://github.com/david-engelmann/atproto/pull/117)
(function-level odoc on public entry points),
[#116](https://github.com/david-engelmann/atproto/pull/116) (live
TestNetwork `updateHandle` / deactivate-activate / drafts /
`getListFeed` / `muteActorList` / `putPreferencesV2`),
[#114](https://github.com/david-engelmann/atproto/pull/114) (odoc landing
page, Dependabot, lexicon-pin drift CI, CODEOWNERS / templates),
[#113](https://github.com/david-engelmann/atproto/pull/113) (live GitHub
Pages URL https://david-engelmann.github.io/atproto/ +
`Label.subscribe_url` localhost `ws://`),
[#112](https://github.com/david-engelmann/atproto/pull/112) (odoc HTML
deploy to GitHub Pages on main),
[#111](https://github.com/david-engelmann/atproto/pull/111) (docs/repo
cleanup),
[#110](https://github.com/david-engelmann/atproto/pull/110)
(Jetstream v2 `Sec-WebSocket-Protocol: xrpc.v1.json`),
[#109](https://github.com/david-engelmann/atproto/pull/109) (live Jetstream
dict-zstd), [#108](https://github.com/david-engelmann/atproto/pull/108)
(lexicon pin `60c4395951`, coverage gate, OCaml `< 5.0`, odoc HTML CI
artifact), changelog [#107](https://github.com/david-engelmann/atproto/pull/107),
and [#106](https://github.com/david-engelmann/atproto/pull/106).

This package is **not** published to the public
[opam-repository](https://github.com/ocaml/opam-repository). Depend on it by
pinning the GitHub repository (see the README). Requires OCaml `>= 4.14.1`
and `< 5.0`.

## 0.1.0 — 2026-09-02

First installable opam package (`dune-project` / `atproto.opam` version
`0.1.0`, OCaml `>= 4.14.1` and `< 5.0`). `opam pin add atproto git+https://github.com/david-engelmann/atproto.git`
exposes `(libraries atproto)`. That pin is not an opam-repository publish.

### Protocol client

- XRPC GET/POST (Cohttp) plus HTTP/2 TLS (`Http_client`) for public HTTPS
- Session / JWT (`Auth`, `Session`), including `authFactorToken` /
  `allowTakendown` and typed `getSession`
- AppView: actor, feed, graph, bookmark, notification, labeler, unspecced,
  video (client only — no hosted transcoder), drafts, contacts, age
  assurance
- Chat / DM client (`chat.bsky.*`) with `atproto-proxy` (no OSS chat
  backend in `@atproto/dev-env` 0.6.4)
- Ozone (`tools.ozone.*`) and `com.atproto.admin` clients
- Repo writes and typed record builders (`Repo`, `Records`)
- Identity, DID PLC/web/key, CID/CAR/DAG-CBOR, MST, TID, AT URI, firehose
- Lexicon 1 parse / validate / `to_ocaml`, including bundled official
  documents
- OAuth / DPoP (`Oauth`, `Oauth_scope`): PKCE S256, PAR, token, refresh,
  RFC 7009 revoke, granular scopes, official `app.bsky.auth*` /
  `chat.bsky.authFullChatClient` permission-sets
- Jetstream v2 tail + live dict-zstd `subscribeEvents`
  ([#109](https://github.com/david-engelmann/atproto/pull/109): Jane Street
  `zstandard` v0.16, `getZstdDictionary`, v2 `zstdDictionary=<id>`; no
  invented archive token) + `.jss` v1 decode.
  [#110](https://github.com/david-engelmann/atproto/pull/110): v2 `subscribe`
  / `subscribe_one` offer `Sec-WebSocket-Protocol: xrpc.v1.json` through
  `Websocket.connect ~extra_headers`; RFC 6455 §4.1 fails the handshake
  unless the 101 echoes that exact protocol. Unoffered connections (v1
  `/subscribe`, firehose) are unchanged. v2 stays server-push only (no
  client data frames; v1 `options_update` / `requireHello` are not sent)
- TAP-like local repo sync helpers (`Repo_sync`) — not a hosted Tap
- `site.standard.*` and `com.germnetwork.declaration` record builders

### Local TestNetwork

CI and `make test-pds` start published `@atproto/dev-env@0.6.4`
(`TestNetwork.create()`: PLC + PDS + AppView + Ozone + bsync).
`ATP_REQUIRE_LOCAL_PDS=1` fails hard on real protocol errors.

- Served XRPC on that stack, including APP-2933
  `app.bsky.graph.referencelistoptout`
- Live local OAuth: loopback client-metadata, AS discovery, PAR, browser
  authorize GET, `~api/sign-in` / `~api/consent` with real cookies, token,
  DPoP `getSession`, refresh, RFC 7009 revoke
- AppView service-auth minted from the OAuth DPoP token (`getServiceAuth`)
- Ozone privileged writes as `admin-mod.test` via OAuth DPoP
  `getServiceAuth` + `Ozone.emit_event_service` (DPoP cannot be proxied)
- [#116](https://github.com/david-engelmann/atproto/pull/116): live
  TestNetwork coverage for PDS `com.atproto.identity.updateHandle` and
  `com.atproto.server.deactivateAccount` / `activateAccount`, plus
  AppView drafts (`createDraft` / `getDrafts` / `updateDraft` /
  `deleteDraft`), `app.bsky.feed.getListFeed`,
  `app.bsky.graph.muteActorList` / `getListMutes`, and
  `app.bsky.notification.putPreferencesV2` (skip only when the NSID is
  not served)
- [#119](https://github.com/david-engelmann/atproto/pull/119): live ozone
  `tools.ozone.communication.createTemplate` (plus update/delete) and
  `tools.ozone.set.upsertSet` (plus `addValues`). Ozone 0.3.1 requires
  `createdBy` on `createTemplate`; `Ozone.create_template` /
  `create_template_body` send it (session DID by default) and
  `update_template` sends `updatedBy` the same way
- `com.atproto.server.createAppPassword` POSTs official `{ "name" }`
  (optional `privileged`). This `@atproto/pds` 0.5.x TestNetwork build
  still 500s on that valid body; the local suite keeps an isolated assert

### Packaging and quality

- `public_name atproto`, generated `atproto.opam`, `opam lint`,
  `dune build -p atproto` / `dune runtest -p atproto`
- GitHub Actions `pull_request` is a sibling of `push`
- Node 24 drop-in action majors: `actions/checkout@v5`,
  `actions/cache@v5`, `actions/setup-node@v5`,
  `actions/upload-artifact@v6`
- Unused `open`s are errors (`-w +33 -warn-error +33`) on the library,
  tests, and `examples/offline.ml`
- Module-level odoc on public modules
- [#108](https://github.com/david-engelmann/atproto/pull/108): official
  lexicon pin bluesky-social/atproto `60c4395951` (APP-2933) as
  `lexicons/official-nsids.json`, plus TestSuite `test_lexicon_coverage`
  (client helper / record builder / bundled permission-set / explicit
  skip); OCaml constraint `(and (>= 4.14.1) (< 5.0))` — CI tests 4.14.1
  only; TestSuite `lint-doc` runs `dune build @doc` and uploads HTML as
  the `odoc-html` artifact on pull requests
- [#112](https://github.com/david-engelmann/atproto/pull/112): on push to
  `main`, the same HTML is deployed with GitHub Actions Pages
  (`actions/upload-pages-artifact` + `actions/deploy-pages`). GitHub
  Pages is enabled (Settings → Pages → Source: GitHub Actions). Live
  site: https://david-engelmann.github.io/atproto/
- [#113](https://github.com/david-engelmann/atproto/pull/113):
  `dune-project` `(documentation ...)` and generated `atproto.opam`
  `doc:` point at that live URL. README / CONTRIBUTING no longer say
  Pages is disabled. Module-level odoc on public
  `Jetstream_zstd_dictionary` (checked-in fallback blob from #109).
  `Label.subscribe_url` uses `ws://` for localhost / `127.0.0.1` / `::1`
  (same as `Firehose.subscribe_url`) so a local PDS/Ozone
  `subscribeLabels` is not forced onto `wss://`
- [#114](https://github.com/david-engelmann/atproto/pull/114): `doc/index.mld`
  odoc landing page (`opam pin`, OCaml bound, `{!modules:}` map);
  Dependabot (`github-actions` weekly group + `npm` for
  `/docker/dev-env`); weekday lexicon-pin drift workflow (compare
  bluesky-social/atproto `lexicons/` to the pin SHA — does not
  auto-bump); `.github/CODEOWNERS`, pull-request template, and issue
  template contact / front matter
- [#117](https://github.com/david-engelmann/atproto/pull/117):
  function-level odoc on public entry points (`Identity.resolve_handle`
  / `resolve_did` / `resolve_identity`, `Session.create_session` /
  `get_session`, `Actor.get_profile` / `get_profiles` /
  `get_preferences`, `Feed.search_posts` /
  `Feed.get_author_feed_page`, `Oauth.public_metadata` /
  `Oauth.loopback_client_id`, `Firehose.subscribe_one`). Request test
  fixtures stay callable and are hidden from generated odoc
- [#119](https://github.com/david-engelmann/atproto/pull/119): README
  Quick start (`Identity.resolve_handle` / `Feed.search_posts`, no
  `ATP_AUTH`), `examples/quickstart.ml`, first-class Docs heading, and
  `make doc` (`dune build @doc`)
- [#120](https://github.com/david-engelmann/atproto/pull/120): Dependabot
  github-actions group bump — `actions/checkout@v7`,
  `actions/setup-node@v7`, `actions/upload-artifact@v7`,
  `actions/cache@v6`
- [#121](https://github.com/david-engelmann/atproto/pull/121): drop unused
  leftover `REGISTRY_DB_CONNECTION_STRING` / `REDIS_*` / `POSTGRES_*`
  from `sample.env` (ATP client vars stay). `SECURITY.md` adds a
  supported-versions line for **0.1.0** (OCaml **4.14.x**)
- `examples/offline.ml` typechecks against the public API under
  `dune build` / `dune runtest`

### Not in this release

- Public opam-repository publish
- Hosted public HTTPS client-metadata / production browser login against a
  remote PDS
- Hosted Tap service or video transcoder
- Official OSS chat backend (TestNetwork does not start one)
- Newly published official lexicons after bluesky-social/atproto
  `60c4395951` (APP-2933) — the coverage gate fails until the pin
  snapshot and bindings (or an explicit skip) are updated
- Jetstream archive HTTP download still needs an operator token this
  library does not invent (live compressed `subscribeEvents` and
  `xrpc.v1.json` subprotocol negotiation are implemented)
