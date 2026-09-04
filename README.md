# atproto

OCaml toolkit for the [AT Protocol](https://atproto.com) (XRPC, lexicons, repo sync, identity, AppView, Ozone, chat).

## Install

This library is **not** published to the public [opam-repository](https://github.com/ocaml/opam-repository). Depend on it by pinning this GitHub repo (OCaml **>= 4.14.1 and < 5.0**, package version **0.1.0**). CI tests **4.14.1** only; Jane Street `core` / `async` / `ppx_jane` without a version pin are not treated as OCaml 5-ready:

```shell
opam pin add atproto git+https://github.com/david-engelmann/atproto.git
```

From a local clone:

```shell
opam pin add atproto .
# or install build/test deps without pinning a release
opam install . --deps-only --with-test
dune build -p atproto
```

Jetstream dict-zstd requires Jane Street `zstandard`, which needs system libzstd (Ubuntu/Debian `libzstd-dev`, macOS Homebrew `zstd`) before `opam pin` / `opam install . --deps-only`.

In a dependent `dune` stanza:

```lisp
(libraries atproto)
```

`opam pin` / `opam install .` invoke `dune build -p atproto` (the same build a dependent sees) and install the public `atproto` library. That does not publish the package to opam-repository.

Version notes for **0.1.0** (the packaged surface through [#194](https://github.com/david-engelmann/atproto/pull/194): Repo `blob_ref_to_json` for upload→record (`parse_blob_ref` / `upload_blob` unchanged) on top of [#193](https://github.com/david-engelmann/atproto/pull/193): Repo `delete_record_body` + `apply_writes_parsed` (string `delete_record` / `apply_writes` unchanged) on top of [#192](https://github.com/david-engelmann/atproto/pull/192): Repo create/put Yojson record helpers (`create_record_json` / `put_record_json`; string `create_record` / `put_record` unchanged) on top of [#191](https://github.com/david-engelmann/atproto/pull/191): typed `Ozone.create_activity` encoding (`report_activity_to_json` / `create_activity_typed` / `create_activity_typed_body`; raw Yojson `create_activity` unchanged) on top of [#190](https://github.com/david-engelmann/atproto/pull/190): typed Draft create/update helpers (`draft_to_json` / `create_draft_typed` / `update_draft_typed`) on top of [#189](https://github.com/david-engelmann/atproto/pull/189): Xrpc `x-atproto-bsky-topics` helpers (`topics_header` / `topics_headers` / optional legacy `x-bsky-topics`) on top of [#188](https://github.com/david-engelmann/atproto/pull/188): OAuth live Ozone hop uses `emit_event_service_typed` on top of [#187](https://github.com/david-engelmann/atproto/pull/187): typed `Ozone.emit_event` encoding (`event_to_json` / `subject_to_json` / `emit_event_typed` / `emit_event_service_typed`) on top of [#186](https://github.com/david-engelmann/atproto/pull/186): typed `Ozone.get_account_preferences` via `Actor.preferences` on top of [#185](https://github.com/david-engelmann/atproto/pull/185): typed Actor `putPreferences` encoding on top of [#184](https://github.com/david-engelmann/atproto/pull/184): notes hygiene [#180](https://github.com/david-engelmann/atproto/pull/180) / [#183](https://github.com/david-engelmann/atproto/pull/183), lexicon pin `f0d4877a` for actor interests `updatedAt` [#181](https://github.com/david-engelmann/atproto/pull/181), merge-when-green ignoring lexicon-pin drift [#182](https://github.com/david-engelmann/atproto/pull/182), leftover `interestsPref.updatedAt` live hops [#184](https://github.com/david-engelmann/atproto/pull/184), `preference_kind_to_json` / `put_preferences_typed` so clients can round-trip `interests_pref.updated_at` without hand-building Yojson, and Ozone `getAccountPreferences` returning the same 16-variant `Actor.preference` union; green non-draft same-repo/Dependabot PRs to `main` may auto-squash-merge, see [CONTRIBUTING](.github/CONTRIBUTING.md)) are in [CHANGELOG.md](CHANGELOG.md). Module HTML from `dune build @doc` is uploaded as the `odoc-html` TestSuite artifact on pull requests. On push to `main`, the same HTML is deployed with GitHub Actions Pages (`actions/upload-pages-artifact` + `actions/deploy-pages`) to https://david-engelmann.github.io/atproto/. GitHub Pages is enabled (Settings → Pages → Source: GitHub Actions). `dune-project` `documentation` points at that live URL.

## Quick start

Pin the repo, then resolve a handle and search posts. Neither call needs `ATP_AUTH`:

```shell
opam pin add atproto git+https://github.com/david-engelmann/atproto.git
```

```ocaml
(* public AppView, no ATP_AUTH *)
let did = (Identity.resolve_handle "jay.bsky.team").did
let posts = Feed.search_posts ~q:"atproto" ~limit:5 ()
```

`examples/quickstart.ml` is that flow as a copy-paste executable (`dune exec -- examples/quickstart.exe`).

## Docs

Module HTML is at https://david-engelmann.github.io/atproto/. Build it locally with `dune build @doc` or `make doc`.

## Environment

Create a `.env` (see `sample.env`) with at least:

- `ATP_AUTH` : `EmailAddress:AppPassword`
  - Use an [App Password](https://bsky.app/settings/app-passwords) (email as the username).
- `ATP_HOST` : `bsky.social`
  - PDS / entryway host **without** a scheme (`localhost:2583` for the official local network).
- `ATP_SCHEME` : `https` (default) or `http` for a local stack without TLS.

Optional:

- `BASE_ENDPOINT` : `xrpc` (default)
- `ATP_APPVIEW_HOST` : AppView host without a scheme (`localhost:2584` for `@atproto/dev-env`)
- `ATP_OZONE_HOST` : Ozone host without a scheme (`localhost:2587`)
- `ATP_OZONE_DID` : Ozone service DID (printed by `scripts/local-atproto.sh env`)
- `ATP_AUTH_BOB` / `ATP_AUTH_OZONE` : second PDS account and ozone admin (local network only)

Session creation, repo writes, graph mutes, bookmarks, chat, ozone, and feed helpers need `ATP_AUTH`. Public identity, DID PLC, firehose subscribe, AppView reads (`public.api.bsky.app`), and most `com.atproto.sync.*` reads do **not**.

## Build and test

```shell
opam install . --deps-only --with-test
dune build
dune runtest
```

`dune build` also typechecks `examples/offline.ml` against the current public API (no network, no credentials). `dune runtest` executes that example. A release-style build (what `opam install` / a dependent sees) is `dune build -p atproto` and `dune runtest -p atproto`.

Live Bluesky tests that need credentials are skipped unless `ATP_AUTH` is set to a real `email:app-password` pair (placeholder values in `sample.env` do not count). Public-network tests (handle resolve, PLC directory, `getLatestCommit`, `subscribeRepos`, AppView feed/search/labeler reads) run without auth and skip only if the request itself fails.

## Local AT Protocol network (PDS + AppView + Ozone)

CI and `make test-pds` start Bluesky's official OSS local network — published [`@atproto/dev-env@0.6.4`](https://www.npmjs.com/package/@atproto/dev-env) (`TestNetwork.create()`, the same stack as `make run-dev-env` in [bluesky-social/atproto](https://github.com/bluesky-social/atproto)). This is a **separate** GitHub Actions job on the runner VM (Docker and Node >= 22 are available there). The existing `build` job stays inside `ocaml/opam:ubuntu-22.04` and does not start Docker.

```shell
# start Postgres+Redis + official dev-env, then run PDS / AppView / Ozone tests
make test-pds

# or step by step
./scripts/local-atproto.sh up
./scripts/local-atproto.sh account
eval "$(./scripts/local-atproto.sh env)"
export ATP_REQUIRE_LOCAL_PDS=1
dune exec -- test/test_local_pds.exe
dune exec -- test/test_local_appview.exe
dune exec -- test/test_local_ozone.exe
dune exec -- test/test_local_oauth.exe

./scripts/local-atproto.sh down
```

`scripts/local-pds.sh` is a back-compat wrapper around `scripts/local-atproto.sh`.

Compose file for Postgres/Redis: `docker/dev-env/compose.yaml` (official `postgres:14.4-alpine` on `5433` and `redis:7.0-alpine` on `6380`, matching atproto `packages/dev-infra`). The Node process then starts:

| Service | Port | Package |
| --- | --- | --- |
| PLC | `http://localhost:2582` | `@did-plc/server` |
| PDS | `http://localhost:2583` | `@atproto/pds` |
| AppView | `http://localhost:2584` | `@atproto/bsky` (`app.bsky.*`) |
| Ozone | `http://localhost:2587` | `@atproto/ozone` (`tools.ozone.*`) |
| bsync | (internal) | `@atproto/bsync` |
| introspect | `http://localhost:2581` | dev-env |

Mock accounts from official `generateMockSetup` (not production Bluesky credentials):

- `alice.test` / `hunter2` and `bob.test` / `hunter2` (the suite waits until AppView has indexed both)
- Ozone admin: `admin-mod.test` / `admin-mod-pass` plus the ozone service DID (`ATP_OZONE_DID`)
- Ozone `ADMIN_PASSWORD` in this stack is `admin-pass`; tests use the PDS session + `atproto-proxy` (the library's existing path)

Point the client at the local stack with:

- `ATP_SCHEME=http`
- `ATP_HOST=localhost:2583`
- `ATP_APPVIEW_HOST=localhost:2584`
- `ATP_OZONE_HOST=localhost:2587`
- `ATP_AUTH=alice.test:hunter2`

`test/test_local_oauth.ml` serves a loopback `client-metadata.json`, discovers the PDS authorization server (`.well-known/oauth-protected-resource` + `oauth-authorization-server`), and runs PAR + DPoP against this `@atproto/dev-env` 0.6.4 oauth-provider. Official `http://localhost?redirect_uri=…` is used when the AS rejects a hosted `http://127.0.0.1` client_id (HTTPS is required by the spec except that loopback exception). `Oauth.form_encode` uses URI generic percent-encoding so a loopback `client_id` (`…&scope=atproto%20transition%3Ageneric`) is one form field; path-safe encoding would split on `&` and the AS would derive metadata with only the default `atproto` scope. Hosted `client-metadata.json` and the official loopback `client_id` query both declare `Oauth.default_scope` (`atproto transition:generic`); PAR requests that same string so `transition:generic` is not an undeclared scope. `GET /oauth/authorize` is a browser document navigation (`sec-fetch-mode: navigate`, `sec-fetch-dest: document`, `sec-fetch-site: none`); a bare GET is HTTP 400 HTML (`Missing sec-fetch-mode header`). With those headers the local AS returns HTTP 200 `__authorizeData` and sets `csrf-token`, `dev-id`, and `ses-id` (it does **not** mint a `code` on that GET — the login/consent SPA is still HTML). The library replays those **real** cookies on `/@atproto/oauth-provider/~api/sign-in` + `/consent` (`sec-fetch-mode: same-origin`, `Origin` / `Referer` = issuer, `x-csrf-token` matching the authorize cookie). Inventing a CSRF token is not a substitute. `alice.test` / `hunter2` complete sign-in and consent; consent returns `/oauth/authorize/redirect?code=…`. Token exchange, DPoP `getSession`, DPoP `getServiceAuth` (`aud` = `ATP_APPVIEW_DID`, `lxm` = `getTimeline` / `listNotifications`), refresh (when the AS issues a refresh token), and RFC 7009 revoke are then required (`ATP_REQUIRE_LOCAL_PDS=1`). Authed AppView still does not accept the DPoP access token or a `createSession` `at+jwt`; the client mints a service-auth JWT from the OAuth session and sends that Bearer to `:2584`. `test_live_oauth_ozone` repeats that login as `admin-mod.test` / `admin-mod-pass`, asserts DPoP + `atproto-proxy` `emitEvent` is rejected (`DPoP requests cannot be proxied`), then mints `getServiceAuth` (`aud` = `ATP_OZONE_DID`, `lxm` = `tools.ozone.moderation.emitEvent`) and POSTs `emitEvent` to `:2587` with that Bearer (`Ozone.emit_event_service_typed`). If AppView or Ozone rejects that hop (or the NSID is not served), only that hop is skipped — token assertions stay required. A public HTTPS client-metadata host and a production browser login against a remote PDS are still application-level.

`test/test_local_pds.ml` hits PDS `com.atproto` identity / session / repo / blob / sync / moderation, plus `refreshSession` (refreshJwt Bearer) / `deleteSession` / `getAccountInviteCodes` and a local PLC directory create/update. `Identity.resolve_did` / `resolve_identity` call the XRPC first, then fall back to local PLC (`PLC_ORIGIN`, default `http://localhost:2582` on a local host) because `@atproto/pds` 0.5.x returns `MethodNotImplemented` for those two queries. `test/test_local_appview.ml` hits AppView `app.bsky.actor` / `feed` / `graph` / `notification` / `labeler` / `unspecced` (public reads on `:2584` with no session). Authenticated AppView APIs (`getTimeline`, `getMutes`, `listNotifications`) mint `com.atproto.server.getServiceAuth` (`aud` = AppView DID, `lxm` = the XRPC) and send that JWT to AppView — never the PDS `at+jwt` access token (`InvalidToken: Malformed token`). Extra AppView NSIDs (`getPosts`, `searchActors`, `searchPostsV2`, `getQuotes`, `getRelationships`, `getLists`, `getActorStarterPacks`, `getPreferences`, …) are called only when this AppView implements them. `test/test_local_ozone.ml` hits `tools.ozone.moderation.emitEvent` / `queryEvents` / `queryStatuses` / `getRepo` / `getRecord` / `searchRepos` / `getEvent` / `getReporterStats`, `tools.ozone.server.getConfig`, `tools.ozone.team.listMembers`, `tools.ozone.communication.listTemplates`, `tools.ozone.set.querySets` / `getValues`, `tools.ozone.queue.listQueues`, `tools.ozone.report.queryReports`, and `com.atproto.label.queryLabels` via the PDS + `atproto-proxy` (direct Ozone rejects `at+jwt`). OAuth clients cannot send DPoP through that proxy; `test_live_oauth_ozone` uses `getServiceAuth` + `Ozone.emit_event_service_typed` on `:2587` instead. If the local network is up, a failed protocol call **fails the test**. The suite skips only when it is not aimed at a local host (typical laptop `dune runtest` without Docker/Node). In CI, `ATP_REQUIRE_LOCAL_PDS=1` is set and the stack is required.

`com.atproto.server.createAppPassword` is sent as the official POST `{ "name" }` (optional `privileged`) with `Authorization: Bearer`. This `@atproto/pds` 0.5.x TestNetwork build still 500s (`InternalServerError`) on that valid call; the local PDS suite asserts that isolated 500 so the rest of the file still runs.

### Chat (`chat.bsky.*`)

Pinned `@atproto/dev-env@0.6.4` `TestNetwork.create()` does **not** start a `chat.bsky.app` DM service. `packages/dev-env/src/bin.ts` sets `ozone.chatUrl` to `http://localhost:2590` with the comment `must run separate chat service`. There is no official OSS chat backend in that revision, so this repo does not fake one. Every `chat.bsky.convo.*` / `chat.bsky.actor.*` (and group/moderation/notification) client sends `atproto-proxy: did:web:api.bsky.chat#bsky_chat`, or the session DID-document `#bsky_chat` service (`did:web:<host>#bsky_chat`), or `ATP_CHAT_DID`. Live DM calls skip unless `ATP_AUTH` has a chat/DM OAuth scope (or `ATP_CHAT=1`).

## What this library covers

| Area | Module | Notes |
| --- | --- | --- |
| Session / JWT | `Auth`, `Session` | `createSession` URL uses `ATP_HOST` + `BASE_ENDPOINT`; optional `authFactorToken` / `allowTakendown`; typed `getSession` (`emailConfirmed`, `active`, `status`) |
| AppView actor | `Actor` | Profiles, search, suggestions, get/put preferences (all current `app.bsky.actor.defs#preferences` kinds). Profile views parse pronouns/website, `associated` (chat / germ / activitySubscription), verification, status, `joinedViaStarterPack`, and viewer scoped mutes / knownFollowers |
| AppView feed | `Feed` | Timeline, `getPostThread` (`threadViewPost` / `notFoundPost` / `blockedPost`, optional parent, top-level embed + quote/bookmark counts, `viewer.knownLikers`), `getAuthorFeed` (`filter` knownValues `posts_with_replies` / `posts_no_replies` / `posts_with_media` / `posts_and_author_threads` / `posts_with_video` + `includePins` + public `get_author_feed_page`), reply `grandparentAuthor`, generators, `searchPosts` + `searchPostsV2` (array filters, `detectedQueryLanguages`), quotes, list feed, interactions |
| AppView graph | `Graph` | Follows/blocks/mutes (including `muteActor` `onlyReposts` / `onlyQuoteposts` scoped mutes), lists, starter packs (`listItemsSample` / official `feeds` / `labels`), `searchStarterPacks` + `searchStarterPacksV2`, `getListsWithMembership` / `getStarterPacksWithMembership`, relationships (`blockedByList` / `blockingByList`), known followers |
| Bookmarks | `Bookmark` | `createBookmark` / `deleteBookmark` / `getBookmarks`; bookmark `item` is the feed `#postView` / `#notFoundPost` / `#blockedPost` union |
| Jetstream | `Jetstream` | v2 live tail, collection/DID/kind filters, seq + unix-µs cursors, reconnect/dedupe, v1 `/subscribe` compat; v2 `subscribe` / `subscribe_one` offer **`Sec-WebSocket-Protocol: xrpc.v1.json`** (RFC 6455 §4.1 echo required; unoffered connections unchanged); **live dict-zstd** `subscribeEvents` (`~compress:true`: v2 `zstdDictionary=<id>`, v1 `compress=true` / `Socket-Encoding: zstd`; `getZstdDictionary` over HTTPS with a checked-in production-dict fallback); v2 is server-push only (no client data frames); Network Replay planner + skippable unauthenticated archive HTTP (no invented archive token); `.jss` v1 header / block-index / columnar decode (`~decompress` injection plus built-in `decompress_zstd`) |
| Video | `Video` | `getJobStatus`, `getUploadLimits`, byte upload (`uploadVideo` URL + POST), multipart `startUpload` / `uploadPart` / `finishUpload` / `abortUpload` / `getUploadStatus`, service-auth audience (`did:web:<pds>` + `uploadBlob` lxm), injectable job poll, `video_embed_json`. Client only — no hosted transcoder |
| Unspecced | `Unspecced` | Popular generators, search skeletons, trending topics + `getTrends` / `getTrendsSkeleton`, tagged suggestions, unspecced age-assurance state, suggestion / feed / starter-pack / onboarding / discover / explore / seeMore skeletons, `getPostThreadV2` / `getPostThreadOtherV2`, config |
| Labeler | `Labeler` | `app.bsky.labeler.getServices` |
| Chat / DMs | `Chat` | `chat.bsky.convo.*` including typed message facets/reactions/embeds, **system message data** (`addedBy` / `removedBy` / `approvedBy` / `unlockedBy` / `lockedBy`), `getConvoMembers` (`role` / `addedBy` / `chatDisabled` / `kind` / leftover `profileViewBasic` avatar / associated / viewer / labels / createdAt / verification), `getMessages.relatedProfiles`, `replyTo` union (`messageView` / `deletedMessageView` / `messageBeforeUserJoinedGroupView`), group convo leftover fields (`createdAt` / `joinLink` / `joinRequestCount` / `memberLimit`), `listConvoRequests` `convoView` / `joinRequestConvoView` union, `getLog` message / relatedProfiles / member; `chat.bsky.group` create/add/remove/edit + join links / join requests / mutual groups; notification prefs; actor status / declaration / `chat.bsky.actor.exportAccountData` / delete; moderation views + `subscribeModEvents`; `atproto-proxy` from default `did:web:api.bsky.chat#bsky_chat`, session `#bsky_chat`, or `ATP_CHAT_DID` |
| Ozone | `Ozone` | `tools.ozone.moderation.*` including typed event/subject unions (`modEventMuteReporter` / `ageAssurance*` / `accountEvent` / `scheduleTakedownEvent` / leftover `modEventView` `creatorHandle` / `subjectHandle` / `modTool`, leftover `subjectStatusView` mute/takedown/appeal/age-assurance fields) plus typed `emit_event` encode (`event_to_json` / `subject_to_json` / `mod_tool_to_json` / `emit_event_typed` / `emit_event_service_typed`; raw Yojson `emit_event` unchanged), typed `create_activity` encode (`report_activity_to_json` / `create_activity_typed` / `create_activity_typed_body`; raw Yojson `create_activity` unchanged), subjects/repos/records, timeline, typed `get_account_preferences` (`Actor.preference` list / `app.bsky.actor.defs#preferences`, including `interestsPref.updatedAt`), reporter stats, scheduled actions; plus communication templates, sets, settings, team, safelink, signature, verification, hosting history + `getConfig` (`appview` / `pds` / `blobDivert` / `chat` / `viewer.role` / `verifierDid`); `tools.ozone.queue.*` (list/create/update/delete, moderator assign + `assignmentView.moderator`, `routeReports`) and `tools.ozone.report.*` (query/get, activities, assignments, stats, close/reassign); password sessions send `atproto-proxy` through the PDS; OAuth uses `getServiceAuth` + `emit_event_service` / `emit_event_service_typed` / `query_events_service` / `get_config_service` on the Ozone host |
| Admin | `Admin` | `com.atproto.admin` subject status, account info (`inviteNote` / `invitedBy` / `threatSignatures`), invites, email |
| Repo writes | `Repo`, `Records` | `createRecord` / `putRecord` / `deleteRecord` / `applyWrites` bodies; Yojson `create_record_json` / `put_record_json` (`create_record_body` / `put_record_body`; string `create_record` / `put_record` unchanged); `delete_record_body` + `apply_writes_parsed` (string `delete_record` / `apply_writes` unchanged); `blob_ref_to_json` (`parse_blob_ref` / `upload_blob` unchanged); typed `describeRepo` / `getRecord` / `listRecords` parsers; builders for post/like/repost/follow/block/listblock/list/listitem/starterpack/`referencelistoptout`/profile/status/contentVisibility/verification/threadgate/postgate/generator/labeler/notification declaration / `com.atproto.lexicon.schema` |
| Server | `Server` | describe server (typed), app passwords (`privileged` + typed `#appPassword` parse), invites (`createInviteCode` `forAccount`, `createInviteCodes` `forAccounts`), `reserveSigningKey`, account activate/status (`activateAccount` / `deactivateAccount` / typed `checkAccountStatus` including `repoCommit` / `repoRev` / `repoBlocks`), `createAccount` extras (`did`, `verificationCode` / `verificationPhone`, `plcOp`), `getServiceAuth` (aud may be `did#service`), email confirm/update (`confirmEmail`, `requestEmailConfirmation`, `requestEmailUpdate`, `updateEmail`). Procedures (`createInviteCode(s)`, `revokeAppPassword`, `resetPassword`, `deleteAccount`, `requestPasswordReset`, `requestAccountDelete`) POST JSON bodies per lexicon (they previously used GET) |
| Identity | `Identity`, `Did_plc`, `Did_web`, `Did_key` | resolve + typed `resolveDid` / `resolveIdentity` (`#identityInfo`). When the host returns `MethodNotImplemented` (PDS 0.5.x and current entryway), the client falls back to PLC (`PLC_ORIGIN` or `http://localhost:2582` on a local PDS) / `did:web` and wraps `{ didDoc }` |
| PLC chain | `Did_plc` | Genesis DID, prev CID links, p256 **and k256** ECDSA (low-S, IEEE P1363). Directory URLs accept a host or a full origin (`http://localhost:2582`); `PLC_ORIGIN` overrides the default `https://plc.directory`. Create/update/tombstone operation builders, directory `POST /{did}`, `GET /{did}/data`, and `GET /{did}/log/audit` |
| Sync | `Sync` | `getLatestCommit`, `getRepo` (CAR), public `getBlocks` (bytes/CAR), `listBlobs`, `listRepos`, host/repo status |
| CID / CAR | `Cid`, `Car`, `Dag_cbor` | CIDv1 (including SHA-256 `Cid.create`) + CARv1, blessed CID check, Sync 1.1 streamable pre-order, IPLD JSON → DAG-CBOR (`$link` / `$bytes`) |
| MST | `Mst` | Layer/prefix rules, node parse, CID verify, lookup, insert/delete/walk, firehose-diff inversion **and** forward apply, `diff_ops` (prev tree → next tree), p256/k256 commit sign+verify, pre-order blocks, collection-range proofs |
| Repo sync (TAP-like) | `Repo_sync` | Library-shaped backfill: open/verify repo CAR, walk records, `getRecord` inclusion proof (partial CAR), record-table apply of firehose ops, `#sync` desync, MST-level `apply_commit_tree`, Sync 1.1 pre-order export + collection-subset CAR, offline `write_signed_repo` (JSON → DAG-CBOR → MST → signed commit → CAR). Not a hosted Tap service |
| TID | `Tid` | Record-key / commit-rev identifiers (base32-sortable, official syntax) |
| AT URI | `At_uri` | `at://` parse / serialize |
| Lexicon | `Lexicon` | Parse lexicon-1 JSON (parameters + procedure input/output schemas + `permission-set`), `to_ocaml` codegen (unions emit polymorphic variants), JSON validate, `resolveLexicon` client, small bundled official lexicon documents including `app.bsky.graph.referencelistoptout` and official OAuth permission-sets |
| Temp | `Temp` | `com.atproto.temp.checkHandleAvailability` (available / suggestions union), `checkSignupQueue`, `dereferenceScope`, plus privileged `addReservedHandle` / `requestPhoneVerification` / `revokeAccountCredentials` clients (no invented operator session). Deprecated `fetchLabels` remains `Label.query_labels` |
| Firehose | `Firehose`, `Websocket` | RFC 6455 client (`wss://` and local `ws://`) + `subscribeRepos` frame decode (`#commit`/`#sync`/`#identity`/`#account`/`#info`) |
| OAuth / DPoP | `Oauth`, `Oauth_scope` | PKCE S256, DPoP ES256 + nonce (RFC 9449 `htu` without query/fragment, random `jti`, RFC 7638 `dpop_jkt`), client metadata (`logo_uri` / `tos_uri` / `policy_uri`), PAR (`prompt=create` signup) / token / refresh / RFC 7009 revoke, `require_request_uri_registration`, resource-server `use_dpop_nonce` retry, `expect_sub` / `expires_at`; origin-aware URLs + loopback HTTP issuer; live Cohttp GET/POST (DPoP-Nonce + cookies); oauth-provider `~api` sign-in/consent helpers (real authorize CSRF/device cookies, no invented CSRF); DPoP XRPC (`xrpc_url` / `get_json_dpop` / `xrpc_post_dpop` / `get_service_auth`) so a client mints AppView or Ozone service-auth from the OAuth token and sends it with `Client.get_json ~bearer` / `Ozone.emit_event_service_typed`; DPoP cannot be proxied; local TestNetwork discovery / hosted loopback metadata / PAR / token / getSession / getServiceAuth / AppView getTimeline / Ozone emitEvent / refresh / revoke; granular scope grammar (`repo:`/`rpc:`/`blob:`/`include:`/`transition:`) + official `app.bsky.auth*` / `chat.bsky.authFullChatClient` permission-set parse/expand |
| Labels | `Label` | `queryLabels` + label / query parse (`ver`, `exp`) + `#selfLabels` + typed `#labelValueDefinition` (`severity` / `blurs` / `locales`) |
| XRPC headers | `Xrpc` | `atproto-proxy`, accept-labelers, rate-limit, `x-atproto-bsky-topics` (deprecated `x-bsky-topics`); service-auth JWT mint/verify (ES256/ES256K, `kid`/`jti`/`iat`/`lxm`, `did#service` aud, replay cache) |
| Errors | `Error` | XRPC `{error, message}` including rate limits |
| Syntax | `Syntax` | Handle, DID, NSID, record-key, datetime, language validators |
| Drafts | `Draft` | `app.bsky.draft` create/get/update/delete + typed draft / embed / threadgate / postgate builders plus typed create/update encode (`draft_to_json` / `create_draft_typed` / `update_draft_typed` / `*_typed_body`; raw Yojson `create_draft` / `update_draft` unchanged) |
| Contacts | `Contact` | `app.bsky.contact` phone verify, import, matches, dismiss, sync status, remove data |
| Age assurance | `Ageassurance` | `app.bsky.ageassurance` begin / getConfig / getState + region-rule union; stash `#event` parses `initIp` / `initUa` / `completeIp` / `completeUa` |
| Embeds / facets | `Embed`, `Facet` | Images, external (`readingTime`, `associatedProfiles`, source theme RGB, `associatedRefs`), record, recordWithMedia, video (`presentation` `default`/`gif`), **gallery**, record `#view` union; `getEmbedExternalView`; mention / link / tag parse **and serialize** |
| Notifications | `Notification` | All `listNotifications` known reasons; prefs / prefs-v2 / activity subscriptions / register+unregister push |
| User reports | `Moderation` | `com.atproto.moderation.createReport` (strongRef / repoRef, optional `modTool`, reason-type constants) |
| Crypto / codecs | `K256`, `Base32`, `Base58`, `Base64url`, `Hash`, `Varint` | secp256k1, multibase, CID/CAR varints |
| HTTP helpers | `App`, `Client`, `Cohttp_client`, `Http_client`, `Http_method`, `Request`, `Response`, `User` | Endpoint URLs, shared XRPC GET/POST (Cohttp) + AppView `post_json_appview` service-auth (password `at+jwt`, or OAuth DPoP `Oauth.get_service_auth` + `get_json ~bearer`) + Ozone host/DID env (`ozone_host_from_env` / `ozone_did_from_env`), **HTTP/2 TLS** GET/POST/PUT/DELETE/PATCH via `Http_client` (IPv6 + `Client.get_json_h2` / `Client.post_json_h2` for public HTTPS). Requires HTTPS + ALPN `h2` |
| Sites | `Site` | Official `site.standard` records: document, publication, theme.basic/color, graph recommend + subscription |
| Germ Network | `Germnetwork` | `com.germnetwork.declaration` record (`$bytes` keys, `messageMe` policy) |

## Remaining gaps

These are product-level, not missing protocol cores.

**What 0.1.0 covers.** The packaged client through [#194](https://github.com/david-engelmann/atproto/pull/194): Repo `blob_ref_to_json` for upload→record (`parse_blob_ref` / `upload_blob` unchanged) on top of [#193](https://github.com/david-engelmann/atproto/pull/193): Repo `delete_record_body` + `apply_writes_parsed` (string `delete_record` / `apply_writes` unchanged) on top of [#192](https://github.com/david-engelmann/atproto/pull/192): Repo create/put Yojson record helpers (`create_record_json` / `put_record_json` / `create_record_body` / `put_record_body`; string `create_record` / `put_record` unchanged) on top of [#191](https://github.com/david-engelmann/atproto/pull/191): typed `Ozone.create_activity` encode (`report_activity_to_json` / `create_activity_typed` / `create_activity_typed_body`; raw Yojson `create_activity` unchanged) on top of [#190](https://github.com/david-engelmann/atproto/pull/190): typed Draft create/update helpers (`draft_to_json` / `create_draft_typed` / `update_draft_typed`; raw Yojson unchanged) on top of [#189](https://github.com/david-engelmann/atproto/pull/189): Xrpc `x-atproto-bsky-topics` helpers (`topics_to_string` / `topics_header` / `legacy_topics_header` / `topics_headers` / `parse_topics` / `topics_from_headers`; current header `x-atproto-bsky-topics`, deprecated `x-bsky-topics`; no lexicon pin bump) on top of [#188](https://github.com/david-engelmann/atproto/pull/188): OAuth live Ozone hop uses `emit_event_service_typed` on top of [#187](https://github.com/david-engelmann/atproto/pull/187): typed `Ozone.emit_event` encode on top of [#186](https://github.com/david-engelmann/atproto/pull/186): XRPC, official lexicon pin `f0d4877a` ([#132](https://github.com/david-engelmann/atproto/pull/132)) with a coverage gate, repo sync, identity, AppView, Ozone, chat client, live local OAuth/DPoP, Jetstream v2 (`xrpc.v1.json` + dict-zstd), remaining function-level odoc through [#166](https://github.com/david-engelmann/atproto/pull/166) (leftover `Embed.get_embed_external_view`), live leftover TestNetwork `getListBlocks` / `getStarterPack` / `getFeedGenerator` / `putActivitySubscription` / PLC request-sign-submit / ozone queue assign / report get-close-stats / setting upsert-remove ([#123](https://github.com/david-engelmann/atproto/pull/123)), remaining served leftover NSIDs (`refreshIdentity` / `checkHandleAvailability` / `dereferenceScope` / `resolveLexicon` / throwaway `requestAccountDelete` / `deleteAccount` / `revokeAppPassword` when create is not the known 500, unspecced skeletons / `getSuggested*` / `getTrendsSkeleton`, `app.bsky.ageassurance.*`, ozone team / signature / hosting / queue unassign-route / report assign-activities-reassign-refresh / `set.deleteValues`; [#129](https://github.com/david-engelmann/atproto/pull/129)), live `tools.ozone.moderation.getAccountPreferences` ([#137](https://github.com/david-engelmann/atproto/pull/137)), live leftover TestNetwork hops for remaining `com.atproto.admin` NSIDs ([#150](https://github.com/david-engelmann/atproto/pull/150)), leftover `com.atproto.server` (`createInviteCode(s)` / email confirm / password reset; [#152](https://github.com/david-engelmann/atproto/pull/152)), AppView `sendInteractions` / `describeFeedGenerator` / notification `putPreferences` v1 ([#153](https://github.com/david-engelmann/atproto/pull/153)), ozone `tools.ozone.report.getAssignments` / `unassignModerator` ([#154](https://github.com/david-engelmann/atproto/pull/154)), leftover `com.atproto.temp` operator hops (`addReservedHandle` / `revokeAccountCredentials`; [#162](https://github.com/david-engelmann/atproto/pull/162)), live leftover `getFeedSkeleton` ([#163](https://github.com/david-engelmann/atproto/pull/163)), live leftover unspecced age-assurance hops ([#168](https://github.com/david-engelmann/atproto/pull/168)), odoc landing leftover modules ([#165](https://github.com/david-engelmann/atproto/pull/165)), leftover `Embed.get_embed_external_view` odoc ([#166](https://github.com/david-engelmann/atproto/pull/166)), odoc landing leftover public protocol modules ([#170](https://github.com/david-engelmann/atproto/pull/170)), compiled-only `examples/offline.ml` constructor/parser coverage ([#171](https://github.com/david-engelmann/atproto/pull/171)), installed-package consumer smoke ([#172](https://github.com/david-engelmann/atproto/pull/172)), notes hygiene through [#172](https://github.com/david-engelmann/atproto/pull/172) ([#173](https://github.com/david-engelmann/atproto/pull/173)), libzstd install note ([#174](https://github.com/david-engelmann/atproto/pull/174)), notes hygiene through [#174](https://github.com/david-engelmann/atproto/pull/174) ([#175](https://github.com/david-engelmann/atproto/pull/175)), Error on the odoc landing map plus libzstd install notes ([#176](https://github.com/david-engelmann/atproto/pull/176)), remaining function-level odoc on `Client.get_json_h2` / `Client.post_json_h2` plus `Feed.filter_posts_*` ([#177](https://github.com/david-engelmann/atproto/pull/177)), notes hygiene through [#177](https://github.com/david-engelmann/atproto/pull/177) ([#178](https://github.com/david-engelmann/atproto/pull/178)), merge-when-green squash automation ([#179](https://github.com/david-engelmann/atproto/pull/179); workflow + script + CONTRIBUTING; docs-only allowlist excludes `.github/scripts` and workflows; green non-draft same-repo/Dependabot PRs to `main` may auto-squash-merge, see [CONTRIBUTING](.github/CONTRIBUTING.md)), notes hygiene through [#179](https://github.com/david-engelmann/atproto/pull/179) ([#180](https://github.com/david-engelmann/atproto/pull/180)), lexicon pin `f0d4877a` for actor interests `updatedAt` (`Actor.interests_pref` = `{ tags; updated_at : string option }`; [#181](https://github.com/david-engelmann/atproto/pull/181)), merge-when-green ignoring lexicon-pin drift ([#182](https://github.com/david-engelmann/atproto/pull/182); pin drift is advisory and must not deadlock docs-only or full-mode merges), notes hygiene through [#182](https://github.com/david-engelmann/atproto/pull/182) ([#183](https://github.com/david-engelmann/atproto/pull/183)), leftover `interestsPref.updatedAt` live getPreferences/putPreferences hops ([#184](https://github.com/david-engelmann/atproto/pull/184); assert `updatedAt` when present), typed Actor `putPreferences` encoding ([#185](https://github.com/david-engelmann/atproto/pull/185); `preference_kind_to_json` / `preference_to_json` / `preferences_to_json` / `put_preferences_typed`; optional `interestsPref.updatedAt`; raw Yojson `put_preferences` unchanged), typed `Ozone.get_account_preferences` via `Actor.preferences` ([#186](https://github.com/david-engelmann/atproto/pull/186); `account_preferences.preferences` is `Actor.preference list`; assert typed kinds and `interestsPref.updatedAt` when present), typed `Ozone.emit_event` encode ([#187](https://github.com/david-engelmann/atproto/pull/187); `event_to_json` / `subject_to_json` / `mod_tool_to_json` / `emit_event_typed` / `emit_event_service_typed`; raw Yojson `emit_event` unchanged; does not invent leftover unused emitEvent fields), OAuth live Ozone hop on `emit_event_service_typed` ([#188](https://github.com/david-engelmann/atproto/pull/188); typed `Comment` / `Repo_ref`; skip-if-not-served unchanged), Xrpc `x-atproto-bsky-topics` helpers ([#189](https://github.com/david-engelmann/atproto/pull/189); `topics_to_string` / `topics_header` / `legacy_topics_header` / `topics_headers` / `parse_topics` / `topics_from_headers`; current header `x-atproto-bsky-topics`, deprecated `x-bsky-topics`; no lexicon pin bump), typed Draft create/update encode ([#190](https://github.com/david-engelmann/atproto/pull/190); `draft_to_json` / `create_draft_typed` / `update_draft_typed`; raw Yojson unchanged), typed `Ozone.create_activity` encode ([#191](https://github.com/david-engelmann/atproto/pull/191); `report_activity_to_json` / `create_activity_typed` / `create_activity_typed_body`; raw Yojson `create_activity` unchanged; does not invent leftover unused createActivity fields), Repo create/put Yojson record helpers ([#192](https://github.com/david-engelmann/atproto/pull/192); `create_record_json` / `put_record_json`; string APIs unchanged; no lexicon pin bump), Repo `delete_record_body` + `apply_writes_parsed` ([#193](https://github.com/david-engelmann/atproto/pull/193); string `delete_record` / `apply_writes` unchanged; no lexicon pin bump), Repo `blob_ref_to_json` for upload→record ([#194](https://github.com/david-engelmann/atproto/pull/194); `parse_blob_ref` / `upload_blob` unchanged; no lexicon pin bump), and odoc HTML on GitHub Pages (https://david-engelmann.github.io/atproto/, enabled after [#112](https://github.com/david-engelmann/atproto/pull/112)). Hosted-only `chat.bsky.*` / video transcoder / Tap / phone / contacts / push / unhosted feed generator stay listed not faked (`requestPhoneVerification` is not faked). Pin this GitHub repo; it is **not** on opam-repository. See [CHANGELOG.md](CHANGELOG.md).


- Official lexicons are pinned at bluesky-social/atproto [`f0d4877a03`](https://github.com/bluesky-social/atproto/commit/f0d4877a03dc8ede0d3e9a36d5b72ada63b5d2e0) (`app.bsky.actor.defs#interestsPref` `updatedAt`). `lexicons/official-nsids.json` is the compact NSID snapshot (`query` / `procedure` / `subscription` / `record` / `permission-set`); `scripts/gen-official-nsids.py` rebuilds it against a SHA. TestSuite `test_lexicon_coverage` fails if that pin grows and a public client NSID is missing a helper, record builder, bundled permission-set, or an explicit one-line skip. Hosted-only *servers* (no OSS chat backend, no video transcoder, no Tap host) are not skip reasons. Five deprecated/internal NSIDs are skipped in `lexicons/coverage-skips.json`: `com.atproto.temp.fetchLabels`, `com.atproto.sync.getCheckout`, `com.atproto.sync.getHead`, `com.atproto.sync.notifyOfUpdate`, and `internal.bsky.actor.getProfiles`.
- Hosting a **public HTTPS client-metadata document** and completing a **production browser login** against a remote PDS. Local TestNetwork already runs loopback metadata + PAR + DPoP through token, AppView service-auth, and Ozone privileged writes as `admin-mod.test`.
- A hosted **Tap** service or hosted **video transcoder** (client types and TAP-like repo sync helpers are implemented). TestNetwork is a local PDS + AppView + Ozone stack, not a public host.
- No official **OSS chat** backend in `@atproto/dev-env` 0.6.4 TestNetwork. `chat.bsky.*` clients still send `atproto-proxy`.
- Jetstream archive HTTP **download** still needs an operator token this library does not invent (live dict-zstd `subscribeEvents` and v2 `xrpc.v1.json` are implemented).
- Permissioned data / spaces / LtHash (no stable public spec yet)
- This package is **not** on opam-repository

## Sample usage

```ocaml
(* public AppView, no auth *)
let did = (Identity.resolve_handle "jay.bsky.team").did
let commit = Sync.get_latest_commit did
let discover =
  Feed.get_feed_generator
    ~feed:"at://did:plc:z72i7hdynmk6r22z27h6tvur/app.bsky.feed.generator/whats-hot"
    ()
let author =
  Feed.get_author_feed_page ~actor:"jay.bsky.team" ~limit:5
    ~filter:Feed.filter_posts_no_replies ~include_pins:true ()
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
(* Official loopback client_id for local / TestNetwork development *)
let loopback =
  Oauth.loopback_client_id ~redirect_uri:"http://127.0.0.1:8080/cb" ()
let _ = Oauth.localhost_metadata loopback

(* video byte-upload pipeline — construct URL + embed; POST needs a service token *)
let upload =
  Video.upload_video_url ~did:"did:plc:abc123xyz0001112223333" ~name:"clip.mp4" ()
let embed =
  Video.video_embed_json
    ~video:(`Assoc [ ("$type", `String "blob"); ("mimeType", `String "video/mp4") ])
    ~alt:"demo" ~presentation:"gif" ()
let schema =
  Records.lexicon_schema ~id:"com.example.ping"
    ~defs:(`Assoc [ ("main", `Assoc [ ("type", `String "query") ]) ])
    ()
let start =
  Video.start_upload_body ~size_bytes:1_048_576 ~mime_type:"video/mp4"
    ~name:"clip.mp4" ()

(* firehose: one subscribeRepos frame from the public relay *)
let _header, msg = Firehose.subscribe_one ()

(* Jetstream v2 JSON tail — URL only here; subscribe_one talks to the public WS
   and offers Sec-WebSocket-Protocol: xrpc.v1.json (RFC 6455 §4.1 echo) *)
let _js =
  Jetstream.subscribe_url
    ~filter:
      {
        Jetstream.empty_filter with
        collections = [ "app.bsky.feed.post" ];
        kinds = [ Jetstream.Commit ];
      }
    ()
let _headers = Jetstream.subscribe_extra_headers ()
(* v2 dict-zstd: query zstdDictionary=<id>; header stays xrpc.v1.json *)
let _js_zstd =
  Jetstream.subscribe_url ~compress:true ~zstd_dictionary_id:20260811 ()
let _headers_zstd = Jetstream.subscribe_extra_headers ~compress:true ()

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
let _ = profile.pronouns
let _ = profile.viewer.muted_only_reposts
let prefs = Actor.get_preferences session
let _ = Actor.preferences_to_json prefs
let _ = Graph.mute_actor_body ~actor:"alice.test" ~only_reposts:true ()
let _ =
  Auth.create_session_body ~identifier:"alice.test" ~password:"x"
    ~allow_takendown:true ()
let bookmarks = Bookmark.get_bookmarks session ~limit:10 ()
(* chat always sends atproto-proxy (session #bsky_chat, ATP_CHAT_DID, or default) *)
let convos = Chat.list_convos session ~limit:10 ()
let chat_status = Chat.get_actor_status session ()
let _ = Chat.effective_proxy ~did_doc:(Option.value ~default:`Null session.did_doc) ()

(* site.standard + germnetwork records — local builders, no network *)
let article =
  Site.document ~site:"https://standard.site" ~title:"hello"
    ~published_at:"2026-01-01T00:00:00.000Z" ()
let germ = Germnetwork.declaration ~version:"1.0.0" ~current_key:"key" ()

(* topics header for Client.get_json ~extra — current x-atproto-bsky-topics *)
let _topics = Xrpc.topics_headers [ "news"; "sports" ]

(* HTTP/2 XRPC GET that keeps status + rate-limit headers (skips if ALPN h2 fails) *)
let _h2 =
  Http_client.xrpc_url ~host:"public.api.bsky.app"
    "com.atproto.identity.resolveHandle" ~query:[ ("handle", "bsky.app") ] ()
```
