# Changelog

Notes for the packaged **0.1.0** library. This file records what actually
shipped through [#138](https://github.com/david-engelmann/atproto/pull/138):
official lexicon pin `5c154f9c` and `Ozone.get_account_preferences`
([#132](https://github.com/david-engelmann/atproto/pull/132)), live leftover
TestNetwork NSIDs ([#129](https://github.com/david-engelmann/atproto/pull/129))
plus live `getAccountPreferences`
([#137](https://github.com/david-engelmann/atproto/pull/137)), and remaining
function-level odoc ([#122](https://github.com/david-engelmann/atproto/pull/122)
Graph / Ozone / Repo / Sync / MST / Jetstream / Chat / Bookmark /
Notification, [#125](https://github.com/david-engelmann/atproto/pull/125)
Label / Video / CID/CAR / PLC / Records,
[#126](https://github.com/david-engelmann/atproto/pull/126)
Firehose / Oauth / Session / Auth / Client / Server,
[#128](https://github.com/david-engelmann/atproto/pull/128)
remaining Ozone / Repo_sync,
[#130](https://github.com/david-engelmann/atproto/pull/130) Identity,
[#131](https://github.com/david-engelmann/atproto/pull/131)
User / Cohttp / Base32/58/64url,
[#133](https://github.com/david-engelmann/atproto/pull/133)
Tid / At_uri / Dag_cbor / K256 / Did,
[#134](https://github.com/david-engelmann/atproto/pull/134)
Error / Xrpc / HTTP, [#135](https://github.com/david-engelmann/atproto/pull/135)
Admin, [#136](https://github.com/david-engelmann/atproto/pull/136) Server,
[#138](https://github.com/david-engelmann/atproto/pull/138)
Session / Moderation / Temp).

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
- [#123](https://github.com/david-engelmann/atproto/pull/123): live leftover
  TestNetwork coverage for AppView `app.bsky.graph.getListBlocks`,
  `getStarterPack` / `getStarterPacks`,
  `app.bsky.feed.getFeedGenerator`,
  `app.bsky.notification.putActivitySubscription`; PDS
  `com.atproto.identity.requestPlcOperationSignature` /
  `signPlcOperation` / `submitPlcOperation`; ozone
  `tools.ozone.queue.assignModerator` / `getAssignments`,
  `tools.ozone.report.getReport` / `closeReports` / `getLiveStats` /
  `getHistoricalStats`, and `tools.ozone.setting.upsertOption` /
  `removeOptions` (skip only when the NSID is not served)
- [#129](https://github.com/david-engelmann/atproto/pull/129): live leftover
  TestNetwork coverage after [#123](https://github.com/david-engelmann/atproto/pull/123):
  PDS `com.atproto.identity.refreshIdentity`,
  `com.atproto.temp.checkHandleAvailability` /
  `com.atproto.temp.dereferenceScope`,
  `com.atproto.lexicon.resolveLexicon`, throwaway
  `requestAccountDelete` / `deleteAccount` (email-token InvalidRequest
  skipped), and `revokeAppPassword` only when `createAppPassword` is
  not the known PDS 0.5.x 500; AppView unspecced skeletons /
  `getSuggested*` / `getTrendsSkeleton` and `app.bsky.ageassurance.*`
  if served; ozone `team.addMember` / `updateMember` / `deleteMember`,
  `signature.findCorrelation` / `findRelatedAccounts` / `searchAccounts`,
  `hosting.getAccountHistory`, `queue.unassignModerator` /
  `routeReports`, `report.assignModerator` / `listActivities` /
  `queryActivities` / `reassignQueue` / `refreshStats`, and
  `set.deleteValues` (skip only when the NSID is not served or the
  InvalidRequest / UpstreamFailure is TestNetwork policy)
- [#137](https://github.com/david-engelmann/atproto/pull/137): live
  TestNetwork hop for `tools.ozone.moderation.getAccountPreferences`
  (`Ozone.get_account_preferences`, required `did`). Skip when the NSID
  is not served, MethodNotImplemented, feature-disabled, or
  UpstreamFailure is TestNetwork policy. Does not fake a hosted ozone
  preference store
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
- [#132](https://github.com/david-engelmann/atproto/pull/132): official
  lexicon pin bluesky-social/atproto `5c154f9c` (the commit that added
  `tools.ozone.moderation.getAccountPreferences`) plus
  `Ozone.get_account_preferences` so the coverage gate stays green
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
- [#122](https://github.com/david-engelmann/atproto/pull/122):
  function-level odoc on public Graph / Ozone / Repo / Sync / MST /
  Jetstream / Chat / Bookmark / Notification entry points
  (`get_follows`, `emit_event`, `create_record`, `get_latest_commit`,
  `layer_for_key`, `subscribe_url`, `list_convos`, `get_bookmarks`,
  `list_notifications`). Comment-only; that PR targets `main` in
  parallel
- [#125](https://github.com/david-engelmann/atproto/pull/125):
  function-level odoc on public Label / Video / CID / CAR / PLC /
  Records / Admin / Embed / Facet / Temp / Site / Germnetwork /
  Websocket / Xrpc / Oauth_scope / Syntax / Tid / At_uri / Lexicon
  entry points (`query_labels`, `upload_video_url`, `Cid.create`,
  `Car.parse`, `genesis_operation`, `Records.post`). Comment-only;
  Video helpers stay client URL/body (no hosted transcoder)
- [#126](https://github.com/david-engelmann/atproto/pull/126):
  function-level odoc on remaining public Firehose / Oauth / Session /
  Auth / Client / Server / Did_key / Did_web / K256 / Dag_cbor /
  Hash / Varint / Error / Http_client entry points (`subscribe_url`,
  `pkce_s256`, `refresh_session`, `get_json`, `describe_server`).
  Comment-only
- [#134](https://github.com/david-engelmann/atproto/pull/134):
  function-level odoc on remaining Error / Xrpc / Request / Response /
  Http_client / Http_method / Websocket helpers
  (`parse_error_from_json`, `parse_proxy`, `parse_service_auth`,
  `xrpc_put`, `recv_message`). Comment-only
- [#136](https://github.com/david-engelmann/atproto/pull/136):
  function-level odoc on remaining Server XRPC wrappers
  (`create_account`, `get_account_invite_codes`, `create_invite_code`,
  `create_invite_codes`, `request_account_delete`, `delete_account`,
  `reset_password`, `revoke_app_password`, `request_email_confirmation`,
  `request_email_update`). Comment-only
- [#135](https://github.com/david-engelmann/atproto/pull/135): remaining
  function-level odoc on Admin XRPC wrappers (`get_account_infos`,
  `enable_account_invites` / `disable_account_invites`, `send_email`,
  `get_invite_codes` / `disable_invite_codes`, `delete_account`,
  `update_account_email` / `update_account_handle` /
  `update_account_password` / `update_account_signing_key`).
  Comment-only
- This PR: function-level odoc on remaining Tid / At_uri / Dag_cbor /
  K256 / Did_key / Did_web helpers (`of_int64`, `to_string`, `as_text`,
  `low_s`, `is_did_key`, `is_web_did`). Comment-only
- [#138](https://github.com/david-engelmann/atproto/pull/138):
  function-level odoc on remaining Session / Moderation / Temp helpers
  (`refresh_token_from_session`, `get_session_request`, reason-type
  constants, `create_report_data_from_*`, `check_signup_queue`,
  `dereference_scope`, `add_reserved_handle`,
  `request_phone_verification`, `revoke_account_credentials`).
  Comment-only; hosted-only phone verification stays listed not faked
- `examples/offline.ml` typechecks against the public API under
  `dune build` / `dune runtest`

### Not in this release

- Public opam-repository publish
- Hosted public HTTPS client-metadata / production browser login against a
  remote PDS
- Hosted Tap service or video transcoder
- Official OSS chat backend (TestNetwork does not start one)
- Newly published official lexicons after bluesky-social/atproto
  `5c154f9c` — the coverage gate fails until the pin
  snapshot and bindings (or an explicit skip) are updated
- Jetstream archive HTTP download still needs an operator token this
  library does not invent (live compressed `subscribeEvents` and
  `xrpc.v1.json` subprotocol negotiation are implemented)
