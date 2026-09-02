# Changelog

Notes for the packaged **0.1.0** library. This file records what actually
shipped through pull request [#106](https://github.com/david-engelmann/atproto/pull/106)
(merge `79aeb75c`, 2026-09-01), changelog
[#107](https://github.com/david-engelmann/atproto/pull/107) (merge `3a7bd82f`,
2026-09-02), and
[#108](https://github.com/david-engelmann/atproto/pull/108)
(lexicon pin `60c4395951`, coverage gate, OCaml `< 5.0`, odoc artifact).

This package is **not** published to the public
[opam-repository](https://github.com/ocaml/opam-repository). Depend on it by
pinning the GitHub repository (see the README).

## 0.1.0 — 2026-09-01

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
- Jetstream v2 tail + `.jss` v1 decode (no invented archive token)
- TAP-like local repo sync helpers (`Repo_sync`) — not a hosted Tap
- `site.standard.*` and `com.germnetwork.declaration` record builders

### Local TestNetwork (#90–#106)

CI and `make test-pds` start published `@atproto/dev-env@0.6.4`
(`TestNetwork.create()`: PLC + PDS + AppView + Ozone + bsync).
`ATP_REQUIRE_LOCAL_PDS=1` fails hard on real protocol errors.

- Leftover served XRPC on that stack, including APP-2933
  `app.bsky.graph.referencelistoptout`
- Live local OAuth: loopback client-metadata, AS discovery, PAR, browser
  authorize GET, `~api/sign-in` / `~api/consent` with real cookies, token,
  DPoP `getSession`, refresh, RFC 7009 revoke
- AppView service-auth minted from the OAuth DPoP token (`getServiceAuth`)
- Ozone privileged writes as `admin-mod.test` via OAuth DPoP
  `getServiceAuth` + `Ozone.emit_event_service` (DPoP cannot be proxied)
- `com.atproto.server.createAppPassword` POSTs official `{ "name" }`
  (optional `privileged`). This `@atproto/pds` 0.5.x TestNetwork build
  still 500s on that valid body; the local suite keeps an isolated assert

### Packaging and quality (#101, #106, #107, #108)

- `public_name atproto`, generated `atproto.opam`, `opam lint`,
  `dune build -p atproto` / `dune runtest -p atproto`
- GitHub Actions `pull_request` is a sibling of `push`
- Node 24 drop-in action majors: `actions/checkout@v5`,
  `actions/cache@v5`, `actions/setup-node@v5`,
  `actions/upload-artifact@v6`
- Unused `open`s are errors (`-w +33 -warn-error +33`) on the library,
  tests, and `examples/offline.ml`
- Module-level odoc on public modules; TestSuite `lint-doc` runs
  `dune build @doc` and uploads HTML as the `odoc-html` artifact (no
  GitHub Pages site)
- `examples/offline.ml` typechecks against the public API under
  `dune build` / `dune runtest`
- Official lexicon pin bluesky-social/atproto `60c4395951` (APP-2933) as
  `lexicons/official-nsids.json`, plus TestSuite `test_lexicon_coverage`
  (client helper / record builder / bundled permission-set / explicit skip)
- OCaml constraint `(and (>= 4.14.1) (< 5.0))` — CI tests 4.14.1 only

### Not in this release

- Public opam-repository publish
- Hosted public HTTPS client-metadata / production browser login against a
  remote PDS
- Hosted Tap service or video transcoder
- Official OSS chat backend (TestNetwork does not start one)
- Newly published official lexicons after bluesky-social/atproto
  `60c4395951` (APP-2933) — the coverage gate fails until the pin
  snapshot and bindings (or an explicit skip) are updated
