# atproto

OCaml toolkit for the [AT Protocol](https://atproto.com) (XRPC, lexicons, repo sync, identity, AppView, Ozone, chat).

## Install

This library is **not** published to the public [opam-repository](https://github.com/ocaml/opam-repository). Depend on it by pinning this GitHub repo (OCaml **4.14.1**, package version **0.1.0**):

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

In a dependent `dune` stanza:

```lisp
(libraries atproto)
```

`opam pin` / `opam install .` invoke `dune build -p atproto` (the same build a dependent sees) and install the public `atproto` library. That does not publish the package to opam-repository.

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

`test/test_local_oauth.ml` serves a loopback `client-metadata.json`, discovers the PDS authorization server (`.well-known/oauth-protected-resource` + `oauth-authorization-server`), and runs PAR + DPoP against this `@atproto/dev-env` 0.6.4 oauth-provider. Official `http://localhost?redirect_uri=…` is used when the AS rejects a hosted `http://127.0.0.1` client_id (HTTPS is required by the spec except that loopback exception). `Oauth.form_encode` uses URI generic percent-encoding so a loopback `client_id` (`…&scope=atproto%20transition%3Ageneric`) is one form field; path-safe encoding would split on `&` and the AS would derive metadata with only the default `atproto` scope. PAR requests the same scope the client_id declares (retries `atproto` only if this AS still omits `transition:generic` from loopback metadata). `GET /oauth/authorize` is a browser document navigation (`sec-fetch-mode: navigate`, `sec-fetch-dest: document`, `sec-fetch-site: none`); a bare GET is HTTP 400 HTML (`Missing sec-fetch-mode header`). The library parses oauth-provider `__errorData` / `__authorizeData` hydration. HTML 400 is a protocol or browser-navigation error, not MethodNotImplemented. Token exchange runs when `/@atproto/oauth-provider/~api/sign-in` + `/consent` accept `alice.test` / `hunter2`; otherwise authorize/token stop after required-green PAR. A public HTTPS client-metadata host and a production browser login are still application-level.

`test/test_local_pds.ml` hits PDS `com.atproto` identity / session / repo / blob / sync / moderation, plus `refreshSession` (refreshJwt Bearer) / `deleteSession` / `getAccountInviteCodes` and a local PLC directory create/update. `Identity.resolve_did` / `resolve_identity` call the XRPC first, then fall back to local PLC (`PLC_ORIGIN`, default `http://localhost:2582` on a local host) because `@atproto/pds` 0.5.x returns `MethodNotImplemented` for those two queries. `test/test_local_appview.ml` hits AppView `app.bsky.actor` / `feed` / `graph` / `notification` / `labeler` / `unspecced` (public reads on `:2584` with no session). Authenticated AppView APIs (`getTimeline`, `getMutes`, `listNotifications`) mint `com.atproto.server.getServiceAuth` (`aud` = AppView DID, `lxm` = the XRPC) and send that JWT to AppView — never the PDS `at+jwt` access token (`InvalidToken: Malformed token`). Extra AppView NSIDs (`getPosts`, `searchActors`, `searchPostsV2`, `getQuotes`, `getRelationships`, `getLists`, `getActorStarterPacks`, `getPreferences`, …) are called only when this AppView implements them. `test/test_local_ozone.ml` hits `tools.ozone.moderation.emitEvent` / `queryEvents` / `queryStatuses` / `getRepo` / `getRecord` / `searchRepos` / `getEvent` / `getReporterStats`, `tools.ozone.server.getConfig`, `tools.ozone.team.listMembers`, `tools.ozone.communication.listTemplates`, `tools.ozone.set.querySets` / `getValues`, `tools.ozone.queue.listQueues`, `tools.ozone.report.queryReports`, and `com.atproto.label.queryLabels` via the PDS + `atproto-proxy` (direct Ozone rejects `at+jwt`). If the local network is up, a failed protocol call **fails the test**. The suite skips only when it is not aimed at a local host (typical laptop `dune runtest` without Docker/Node). In CI, `ATP_REQUIRE_LOCAL_PDS=1` is set and the stack is required.

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
| Jetstream | `Jetstream` | v2 live tail, collection/DID/kind filters, seq + unix-µs cursors, reconnect/dedupe, v1 `/subscribe` compat; Network Replay planner + skippable unauthenticated HTTP (no invented archive token); `.jss` v1 header / block-index / columnar decode (zstd via injected callback) |
| Video | `Video` | `getJobStatus`, `getUploadLimits`, byte upload (`uploadVideo` URL + POST), multipart `startUpload` / `uploadPart` / `finishUpload` / `abortUpload` / `getUploadStatus`, service-auth audience (`did:web:<pds>` + `uploadBlob` lxm), injectable job poll, `video_embed_json`. Client only — no hosted transcoder |
| Unspecced | `Unspecced` | Popular generators, search skeletons, trending topics + `getTrends` / `getTrendsSkeleton`, tagged suggestions, unspecced age-assurance state, suggestion / feed / starter-pack / onboarding / discover / explore / seeMore skeletons, `getPostThreadV2` / `getPostThreadOtherV2`, config |
| Labeler | `Labeler` | `app.bsky.labeler.getServices` |
| Chat / DMs | `Chat` | `chat.bsky.convo.*` including typed message facets/reactions/embeds, **system message data** (`addedBy` / `removedBy` / `approvedBy` / `unlockedBy` / `lockedBy`), `getConvoMembers` (`role` / `addedBy` / `chatDisabled` / `kind` / leftover `profileViewBasic` avatar / associated / viewer / labels / createdAt / verification), `getMessages.relatedProfiles`, `replyTo` union (`messageView` / `deletedMessageView` / `messageBeforeUserJoinedGroupView`), group convo leftover fields (`createdAt` / `joinLink` / `joinRequestCount` / `memberLimit`), `listConvoRequests` `convoView` / `joinRequestConvoView` union, `getLog` message / relatedProfiles / member; `chat.bsky.group` create/add/remove/edit + join links / join requests / mutual groups; notification prefs; actor status / declaration / `chat.bsky.actor.exportAccountData` / delete; moderation views + `subscribeModEvents`; `atproto-proxy` from default `did:web:api.bsky.chat#bsky_chat`, session `#bsky_chat`, or `ATP_CHAT_DID` |
| Ozone | `Ozone` | `tools.ozone.moderation.*` including typed event/subject unions (`modEventMuteReporter` / `ageAssurance*` / `accountEvent` / `scheduleTakedownEvent` / leftover `modEventView` `creatorHandle` / `subjectHandle` / `modTool`, leftover `subjectStatusView` mute/takedown/appeal/age-assurance fields), subjects/repos/records, timeline, reporter stats, scheduled actions; plus communication templates, sets, settings, team, safelink, signature, verification, hosting history + `getConfig` (`appview` / `pds` / `blobDivert` / `chat` / `viewer.role` / `verifierDid`); `tools.ozone.queue.*` (list/create/update/delete, moderator assign + `assignmentView.moderator`, `routeReports`) and `tools.ozone.report.*` (query/get, activities, assignments, stats, close/reassign); requires `atproto-proxy` |
| Admin | `Admin` | `com.atproto.admin` subject status, account info (`inviteNote` / `invitedBy` / `threatSignatures`), invites, email |
| Repo writes | `Repo`, `Records` | `createRecord` / `putRecord` / `deleteRecord` / `applyWrites` bodies; typed `describeRepo` / `getRecord` / `listRecords` parsers; builders for post/like/repost/follow/block/listblock/list/listitem/starterpack/`referencelistoptout`/profile/status/contentVisibility/verification/threadgate/postgate/generator/labeler/notification declaration / `com.atproto.lexicon.schema` |
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
| OAuth / DPoP | `Oauth`, `Oauth_scope` | PKCE S256, DPoP ES256 + nonce (RFC 9449 `htu` without query/fragment, random `jti`, RFC 7638 `dpop_jkt`), client metadata (`logo_uri` / `tos_uri` / `policy_uri`), PAR (`prompt=create` signup) / token / RFC 7009 revoke, `require_request_uri_registration`, resource-server `use_dpop_nonce` retry, `expect_sub` / `expires_at`; origin-aware URLs + loopback HTTP issuer; live Cohttp GET/POST (DPoP-Nonce + cookies); local TestNetwork discovery / hosted loopback metadata / PAR; granular scope grammar (`repo:`/`rpc:`/`blob:`/`include:`/`transition:`) + official `app.bsky.auth*` / `chat.bsky.authFullChatClient` permission-set parse/expand |
| Labels | `Label` | `queryLabels` + label / query parse (`ver`, `exp`) + `#selfLabels` + typed `#labelValueDefinition` (`severity` / `blurs` / `locales`) |
| XRPC headers | `Xrpc` | `atproto-proxy`, accept-labelers, rate-limit; service-auth JWT mint/verify (ES256/ES256K, `kid`/`jti`/`iat`/`lxm`, `did#service` aud, replay cache) |
| Errors | `Error` | XRPC `{error, message}` including rate limits |
| Syntax | `Syntax` | Handle, DID, NSID, record-key, datetime, language validators |
| Drafts | `Draft` | `app.bsky.draft` create/get/update/delete + typed draft / embed / threadgate / postgate builders |
| Contacts | `Contact` | `app.bsky.contact` phone verify, import, matches, dismiss, sync status, remove data |
| Age assurance | `Ageassurance` | `app.bsky.ageassurance` begin / getConfig / getState + region-rule union; stash `#event` parses `initIp` / `initUa` / `completeIp` / `completeUa` |
| Embeds / facets | `Embed`, `Facet` | Images, external (`readingTime`, `associatedProfiles`, source theme RGB, `associatedRefs`), record, recordWithMedia, video (`presentation` `default`/`gif`), **gallery**, record `#view` union; `getEmbedExternalView`; mention / link / tag parse **and serialize** |
| Notifications | `Notification` | All `listNotifications` known reasons; prefs / prefs-v2 / activity subscriptions / register+unregister push |
| User reports | `Moderation` | `com.atproto.moderation.createReport` (strongRef / repoRef, optional `modTool`, reason-type constants) |
| Crypto / codecs | `K256`, `Base32`, `Base58`, `Base64url`, `Hash`, `Varint` | secp256k1, multibase, CID/CAR varints |
| HTTP helpers | `App`, `Client`, `Cohttp_client`, `Http_client`, `Http_method`, `Request`, `Response`, `User` | Endpoint URLs, shared XRPC GET/POST (Cohttp) + AppView `post_json_appview` service-auth, **HTTP/2 TLS** GET/POST/PUT/DELETE/PATCH via `Http_client` (IPv6 + `Client.get_json_h2` / `Client.post_json_h2` for public HTTPS). Requires HTTPS + ALPN `h2` |
| Sites | `Site` | Official `site.standard` records: document, publication, theme.basic/color, graph recommend + subscription |
| Germ Network | `Germnetwork` | `com.germnetwork.declaration` record (`$bytes` keys, `messageMe` policy) |

## Remaining gaps

These are product-level, not missing protocol cores:

- Hosting a **public HTTPS client-metadata document** and completing a **production browser login** against a remote PDS. Local TestNetwork now serves a loopback metadata document and runs discovery + PAR + DPoP in CI (`test/test_local_oauth.ml`). `GET /oauth/authorize` is sent as a document navigation (`Sec-Fetch-*`); the local AS still serves an HTML login/consent SPA, so the authorization code is minted only when the oauth-provider sign-in/consent APIs complete with `alice.test` / `hunter2`.
- A hosted Tap service, hosted video transcoder, or live Ozone operator session (client request/response types, video byte-upload + job poll, TAP-like repo sync helpers, and proxy headers are implemented). A **local PDS + PLC** stack is included for `com.atproto.*` integration tests; it is not a public host.
- Jetstream Network Replay / HTTP snapshot **download** against Bluesky's gated archive (planner, `listSegments` types, cutover cursor, Range resume, skippable unauthenticated HTTP, and `.jss` v1 decode are implemented; a live archive download still needs an operator token this library does not invent, and zstd frames need an injected decompressor)
- Permissioned data / spaces / LtHash (no stable public spec to implement yet)
- Official `com.atproto.sync.getRepo` **lexicon** still has no `collection` parameter (client-side subset export from a full CAR is implemented; servers that reject unknown query params are unchanged)

#70–#90 covered protocol core, AppView/chat/ozone/temp, Jetstream, video, OAuth scopes, thread v2 / drafts / contacts, remaining preference kinds, ozone queue/report, `site.standard.*`, leftover admin, HTTP/2, server email/activate, leftover official field parsers, and the official `@atproto/dev-env` local network in CI.

This stack fills leftover *library* holes after #90–#100: live local OAuth (loopback client-metadata + AS discovery + PAR/DPoP against TestNetwork), official `app.bsky.graph.referencelistoptout`, OAuth PAR `prompt=create` / RFC 7638 `dpop_jkt` / RFC 7009 revoke, typed official permission-set lexicons (`include:app.bsky.auth*`), `Client.post_json_h2`, DAG-CBOR IPLD JSON (`$link`/`$bytes`), offline `Repo_sync.write_signed_repo`, and more local PDS/AppView/Ozone XRPC the stack actually serves. Chat still has no OSS server in TestNetwork; skippable live `chat.bsky.*` tests keep `atproto-proxy`.

Privileged admin/ozone writes still need a real operator session and are not invented here.

Still leftover vs current lexicons (not invented here):

- Deprecated `com.atproto.temp.fetchLabels` (use `Label.query_labels` / `subscribeLabels`)
- Deprecated `com.atproto.sync.getCheckout` / `getHead` (use `getRepo` / `getLatestCommit`)
- `internal.bsky.actor.getProfiles` (service-to-service AppView query, not a public client surface)
- Internal `debug` fields and deprecated `entities` / `isFallback`
- Privileged Ozone operator view fields (clients exist; live operator session not invented)
- `com.atproto.server.createAppPassword` 500 on this `@atproto/pds` 0.5.x TestNetwork build (library request matches the official lexicon; the local suite asserts the isolated 500)

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
let _ = profile.pronouns
let _ = profile.viewer.muted_only_reposts
let prefs = Actor.get_preferences session
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

(* HTTP/2 XRPC GET that keeps status + rate-limit headers (skips if ALPN h2 fails) *)
let _h2 =
  Http_client.xrpc_url ~host:"public.api.bsky.app"
    "com.atproto.identity.resolveHandle" ~query:[ ("handle", "bsky.app") ] ()
```
