# atproto

OCaml toolkit for the [AT Protocol](https://atproto.com) (XRPC, lexicons, repo sync, identity, AppView, Ozone, chat).

## Status

**1.0.0** is the packaged surface. It is maintained for third-party use. See [CHANGELOG.md](CHANGELOG.md) (`## [1.0.0]`) for what is new since tagged **0.1.0** ([#229](https://github.com/david-engelmann/atproto/pull/229)–[#236](https://github.com/david-engelmann/atproto/pull/236)) and [Remaining gaps](#remaining-gaps) for hosted-only products this client does not fake. Tagging / GitHub Release and the opam-repository 1.0.0 PR remain maintainer follow-ups.

## Install

This library is published to the public [opam-repository](https://github.com/ocaml/opam-repository) (`opam install atproto`). Pin this GitHub repo for the **1.0.0** surface (OCaml **>= 4.14.1 and < 5.4**, package version **1.0.0**). The 0.1.0 opam-repository PR is separate; a 1.0.0 opam publish is a follow-up after the maintainer tags. CI `build` tests **4.14.1** and **5.3.0**. Jane Street `core` / `async` / `ppx_jane` / `zstandard` are pinned to **>= v0.16.0 and < v0.18~** (v0.16 on OCaml 4.14, v0.17 — the 5-ready line — on 5.1–5.3). Public Jane Street v0.17 does not support OCaml 5.4+; 5.0 is untested (v0.17 needs 5.1+). ocamlformat **0.25.1** stays the project format (`lint-fmt` on 4.14.1; that release needs OCaml `< 5.2`):

```shell
opam install atproto
# development pin
opam pin add atproto git+https://github.com/david-engelmann/atproto.git
```

From a local clone:

```shell
opam pin add atproto .
# or install build/test deps without pinning a release
opam install . --deps-only --with-test
dune build -p atproto
```

Jetstream dict-zstd still requires Jane Street `zstandard` and system libzstd (Ubuntu/Debian `libzstd-dev`, macOS Homebrew `zstd`) before `opam pin` / `opam install . --deps-only`. The Jane Street `zstandard` opam package is Linux-only (x86_64 / arm64) in both v0.16 and v0.17.

In a dependent `dune` stanza:

```lisp
(libraries atproto)
```

`opam pin` / `opam install .` invoke `dune build -p atproto` (the same build a dependent sees) and install the public `atproto` library. A GitHub pin is for development; the published package is `opam install atproto`.

Version notes for **1.0.0** are in [CHANGELOG.md](CHANGELOG.md) (`## [1.0.0] - 2026-09-09`). The packaged surface is the protocol client through [#236](https://github.com/david-engelmann/atproto/pull/236); hosted-only products stay listed under [Remaining gaps](#remaining-gaps). No lexicon pin bump (official pin stays `f0d4877a`). Tagged **0.1.0** (`8f44fb9` / [#228](https://github.com/david-engelmann/atproto/pull/228)) remains the prior RC.

## Quick start

Install the package, then resolve a handle and search posts. Neither call needs `ATP_AUTH`:

```shell
opam install atproto
# or pin this repo for development
opam pin add atproto git+https://github.com/david-engelmann/atproto.git
```

```ocaml
(* public AppView, no ATP_AUTH *)
let did = (Identity.resolve_handle "jay.bsky.team").did
let posts = Feed.search_posts ~q:"atproto" ~limit:5 ()
```

`examples/quickstart.ml` is that flow as a copy-paste executable (`dune exec -- examples/quickstart.exe`).

## Docs

Module HTML is at https://david-engelmann.github.io/atproto/. Build it locally with `dune build @doc` or `make doc`. PRs upload the `odoc-html` TestSuite artifact; pushes to `main` deploy with GitHub Actions Pages (`actions/upload-pages-artifact` + `actions/deploy-pages`). GitHub Pages is enabled (Settings → Pages → Source: GitHub Actions). `dune-project` `documentation` points at that live URL.

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
- `ATP_CHAT_DID` : chat `atproto-proxy` DID (`did:web:api.bsky.chat#bsky_chat` by default; a bare DID gets `#bsky_chat`)
- `ATP_CHAT_HOST` : hosted chat XRPC host without a scheme (`api.bsky.chat` by default)
- `ATP_CHAT` : set to `1` / `true` / `yes` / `on` to run live DM tests even when the session JWT scope does not look like a chat grant
- `ATP_VIDEO_HOST` : hosted video XRPC host without a scheme (`video.bsky.app` by default)

Session creation, repo writes, graph mutes, bookmarks, chat, ozone, and feed helpers need `ATP_AUTH`. Chat additionally needs a privileged / DM-capable session (`transition:chat.bsky`, `include:chat.bsky.authFullChatClient`, or `ATP_CHAT=1`). Live video-limit calls skip unless `ATP_AUTH` is a real credential (no invented video product). Public identity, DID PLC, firehose subscribe, AppView reads (`public.api.bsky.app`), and most `com.atproto.sync.*` reads do **not**.

## Build and test

```shell
opam install . --deps-only --with-test
dune build
dune runtest
```

`dune build` also typechecks `examples/offline.ml` against the current public API (no network, no credentials). `dune runtest` executes that example plus `examples/oauth_https_metadata.ml`, `examples/chat_production.ml`, and `examples/video_production.ml`. A release-style build (what `opam install` / a dependent sees) is `dune build -p atproto` and `dune runtest -p atproto`.

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

`test/test_local_oauth.ml` serves a loopback `client-metadata.json`, discovers the PDS authorization server (`.well-known/oauth-protected-resource` + `oauth-authorization-server`), and runs PAR + DPoP against this `@atproto/dev-env` 0.6.4 oauth-provider. Official `http://localhost?redirect_uri=…` is used when the AS rejects a hosted `http://127.0.0.1` client_id (HTTPS is required by the spec except that loopback exception). `Oauth.form_encode` uses URI generic percent-encoding so a loopback `client_id` (`…&scope=atproto%20transition%3Ageneric`) is one form field; path-safe encoding would split on `&` and the AS would derive metadata with only the default `atproto` scope. Hosted `client-metadata.json` and the official loopback `client_id` query both declare `Oauth.default_scope` (`atproto transition:generic`); PAR requests that same string so `transition:generic` is not an undeclared scope. `GET /oauth/authorize` is a browser document navigation (`sec-fetch-mode: navigate`, `sec-fetch-dest: document`, `sec-fetch-site: none`); a bare GET is HTTP 400 HTML (`Missing sec-fetch-mode header`). With those headers the local AS returns HTTP 200 `__authorizeData` and sets `csrf-token`, `dev-id`, and `ses-id` (it does **not** mint a `code` on that GET — the login/consent SPA is still HTML). The library replays those **real** cookies on `/@atproto/oauth-provider/~api/sign-in` + `/consent` (`sec-fetch-mode: same-origin`, `Origin` / `Referer` = issuer, `x-csrf-token` matching the authorize cookie). Inventing a CSRF token is not a substitute. `alice.test` / `hunter2` complete sign-in and consent; consent returns `/oauth/authorize/redirect?code=…`. Token exchange, DPoP `getSession`, DPoP `getServiceAuth` (`aud` = `ATP_APPVIEW_DID`, `lxm` = `getTimeline` / `listNotifications`), refresh (when the AS issues a refresh token), and RFC 7009 revoke are then required (`ATP_REQUIRE_LOCAL_PDS=1`). Authed AppView still does not accept the DPoP access token or a `createSession` `at+jwt`; the client mints a service-auth JWT from the OAuth session and sends that Bearer to `:2584`. `test_live_oauth_ozone` repeats that login as `admin-mod.test` / `admin-mod-pass`, asserts DPoP + `atproto-proxy` `emitEvent` is rejected (`DPoP requests cannot be proxied`), then mints `getServiceAuth` (`aud` = `ATP_OZONE_DID`, `lxm` = `tools.ozone.moderation.emitEvent`) and POSTs `emitEvent` to `:2587` with that Bearer (`Ozone.emit_event_service_typed`). If AppView or Ozone rejects that hop (or the NSID is not served), only that hop is skipped — token assertions stay required. A public HTTPS client-metadata host is still application-level: `Oauth.public_https_metadata` / `start_browser_login` / `complete_browser_login` build and drive the authorize → code → token path; the app publishes the HTTPS document and receives the browser redirect.

`test/test_local_pds.ml` hits PDS `com.atproto` identity / session / repo / blob / sync / moderation, plus `refreshSession` (refreshJwt Bearer) / `deleteSession` / `getAccountInviteCodes` and a local PLC directory create/update. `Identity.resolve_did` / `resolve_identity` call the XRPC first, then fall back to local PLC (`PLC_ORIGIN`, default `http://localhost:2582` on a local host) because `@atproto/pds` 0.5.x returns `MethodNotImplemented` for those two queries. `test/test_local_appview.ml` hits AppView `app.bsky.actor` / `feed` / `graph` / `notification` / `labeler` / `unspecced` (public reads on `:2584` with no session). Authenticated AppView APIs (`getTimeline`, `getMutes`, `listNotifications`) mint `com.atproto.server.getServiceAuth` (`aud` = AppView DID, `lxm` = the XRPC) and send that JWT to AppView — never the PDS `at+jwt` access token (`InvalidToken: Malformed token`). Extra AppView NSIDs (`getPosts`, `searchActors`, `searchPostsV2`, `getQuotes`, `getRelationships`, `getLists`, `getActorStarterPacks`, `getPreferences`, …) are called only when this AppView implements them. `test/test_local_ozone.ml` hits `tools.ozone.moderation.emitEvent` / `queryEvents` / `queryStatuses` / `getRepo` / `getRecord` / `searchRepos` / `getEvent` / `getReporterStats`, `tools.ozone.server.getConfig`, `tools.ozone.team.listMembers`, `tools.ozone.communication.listTemplates`, `tools.ozone.set.querySets` / `getValues`, `tools.ozone.queue.listQueues`, `tools.ozone.report.queryReports`, and `com.atproto.label.queryLabels` via the PDS + `atproto-proxy` (direct Ozone rejects `at+jwt`). OAuth clients cannot send DPoP through that proxy; `test_live_oauth_ozone` uses `getServiceAuth` + `Ozone.emit_event_service_typed` on `:2587` instead. If the local network is up, a failed protocol call **fails the test**. The suite skips only when it is not aimed at a local host (typical laptop `dune runtest` without Docker/Node). In CI, `ATP_REQUIRE_LOCAL_PDS=1` is set and the stack is required.

`com.atproto.server.createAppPassword` is sent as the official POST `{ "name" }` (optional `privileged`) with `Authorization: Bearer`. This `@atproto/pds` 0.5.x TestNetwork build still 500s (`InternalServerError`) on that valid call; the local PDS suite asserts that isolated 500 so the rest of the file still runs.

### Chat (`chat.bsky.*`)

Pinned `@atproto/dev-env@0.6.4` `TestNetwork.create()` does **not** start a `chat.bsky.app` DM service. `packages/dev-env/src/bin.ts` sets `ozone.chatUrl` to `http://localhost:2590` with the comment `must run separate chat service`. There is no official OSS chat backend in that revision, so this repo does not fake one.

The **production path** is the hosted Bluesky chat service (`did:web:api.bsky.chat#bsky_chat`, host `api.bsky.chat` / `ATP_CHAT_HOST`). See [Production chat](#production-chat-hosted-chatbsky) below. Live DM calls skip unless `ATP_AUTH` has a chat/DM OAuth scope (or `ATP_CHAT=1`).

### Video (`app.bsky.video.*`)

Pinned `@atproto/dev-env@0.6.4` `TestNetwork.create()` does **not** start a `video.bsky.app` transcoder. This repo does not fake one.

The **production path** is the hosted Bluesky video service (host `video.bsky.app` / `ATP_VIDEO_HOST`). See [Production video](#production-video-hosted-appbskyvideo) below. Live `getUploadLimits` skips unless `ATP_AUTH` is a real credential.

## What this library covers

| Area | Module | Notes |
| --- | --- | --- |
| Session / JWT | `Auth`, `Session` | `createSession` URL uses `ATP_HOST` + `BASE_ENDPOINT`; optional `authFactorToken` / `allowTakendown`; typed `getSession` (`emailConfirmed`, `active`, `status`) via `Client.get_text`; `refreshSession` / `deleteSession` via `Client.post_json` with Bearer `refreshJwt` (empty delete output stays `""`) |
| AppView actor | `Actor` | Profiles, search, suggestions via `Client.get_json` (`get_profile` / `get_profiles` / `get_suggestions` / `search_actors` / `search_actors_typeahead` share `get_profile_body` / `get_profiles_body` / `get_suggestions_body` / `search_actors_body`), get/put preferences (all current `app.bsky.actor.defs#preferences` kinds). Profile views parse pronouns/website, `associated` (chat / germ / activitySubscription), verification, status, `joinedViaStarterPack`, and viewer scoped mutes / knownFollowers |
| AppView feed | `Feed` | Timeline, `getPostThread` (`threadViewPost` / `notFoundPost` / `blockedPost`, optional parent, top-level embed + quote/bookmark counts, `viewer.knownLikers`), `getAuthorFeed` (`filter` knownValues `posts_with_replies` / `posts_no_replies` / `posts_with_media` / `posts_and_author_threads` / `posts_with_video` + `includePins` + public `get_author_feed_page`), session `get_author_feed` / `get_likes` / `get_post_thread` / `get_posts` / `get_reposted_by` / `get_timeline` / `get_feed_skeleton` via `Client.get_json` / `Client.get_text` sharing query-pair helpers, leftover `get_author_feed` / `get_author_feed_page` sharing `get_author_feed_body` and `get_feed_skeleton` / `get_feed_skeleton_parsed` sharing `get_feed_skeleton_body`, leftover AppView `get_feed` / `get_list_feed` / `get_actor_feeds` / `search_posts` sharing `get_feed_body` / `get_list_feed_body` / `get_actor_feeds_body` / `search_posts_body`, leftover `search_posts_v2` / `get_quotes` / `get_actor_likes` sharing `search_posts_v2_body` / `get_quotes_body` / `get_actor_likes_body`, reply `grandparentAuthor`, generators, `searchPosts` + `searchPostsV2` (array filters, `detectedQueryLanguages`), quotes, list feed, interactions |
| AppView graph | `Graph` | Follows/blocks/mutes (including `muteActor` `onlyReposts` / `onlyQuoteposts` scoped mutes; `mute_actor_body` / `unmute_actor_body`; `mute_actor` / `unmute_actor` via `Client.post_json` keep `string`; `mute_actor_list_body` / `unmute_actor_list_body` / `mute_thread_body` / `unmute_thread_body`; `mute_actor_list` / `unmute_actor_list` / `mute_thread` / `unmute_thread` share those bodies via `Client.post_json`), leftover AppView `get_list` / `get_lists` / `get_actor_starter_packs` / `search_starter_packs` / `search_starter_packs_v2` / `get_relationships` / `get_known_followers` sharing `get_list_body` / `get_lists_body` / `get_actor_starter_packs_body` / `search_starter_packs_body` / `get_relationships_body` / `get_known_followers_body`, lists, starter packs (`listItemsSample` / official `feeds` / `labels`), `searchStarterPacks` + `searchStarterPacksV2`, `getListsWithMembership` / `getStarterPacksWithMembership`, relationships (`blockedByList` / `blockingByList`), known followers |
| Bookmarks | `Bookmark` | `createBookmark` / `deleteBookmark` / `getBookmarks`; bookmark `item` is the feed `#postView` / `#notFoundPost` / `#blockedPost` union |
| Jetstream | `Jetstream` | v2 live tail, collection/DID/kind filters, seq + unix-µs cursors, reconnect/dedupe, v1 `/subscribe` compat; v2 `subscribe` / `subscribe_one` offer **`Sec-WebSocket-Protocol: xrpc.v1.json`** (RFC 6455 §4.1 echo required; unoffered connections unchanged); **live dict-zstd** `subscribeEvents` (`~compress:true`: v2 `zstdDictionary=<id>`, v1 `compress=true` / `Socket-Encoding: zstd`; `getZstdDictionary` over HTTPS with a checked-in production-dict fallback); v2 is server-push only (no client data frames); Network Replay planner + archive HTTP (`JETSTREAM_API_KEY` / `JETSTREAM_ARCHIVE_TOKEN` or `~token` → `Authorization: Bearer`; skippable unauthenticated path for public/self-hosted; `require_archive_token` / `~require_token` if the operator needs a key and has not supplied one; no invented key); `.jss` v1 header / block-index / columnar decode (`~decompress` injection plus built-in `decompress_zstd`) |
| Video | `Video` | `getJobStatus`, `getUploadLimits` / `get_upload_limits_service`, byte upload (`uploadVideo` URL + POST), multipart `startUpload` / `uploadPart` / `finishUpload` / `abortUpload` / `getUploadStatus`, service-auth audience (`pds_audience` from session `#atproto_pds` else `atp_host`; `uploadBlob` lxm; `upload_service_auth_body` / `mint_upload_token`), query-pair helpers (`get_job_status_body` / `get_upload_status_body` / `upload_video_body`), injectable job poll, `video_embed_json` / `embed_of_blob` / `embed_of_job` for `Records.post`, `part_slice` for multipart. Client only — no hosted transcoder |
| Unspecced | `Unspecced` | Popular generators, leftover search skeletons sharing `search_posts_skeleton_body` / `search_actors_skeleton_body` / `search_starter_packs_skeleton_body`, leftover `get_suggestions_skeleton` / `get_post_thread_v2` / suggested-users sharing `get_suggestions_skeleton_body` / `get_post_thread_v2_body` / `get_suggested_users_body` / `get_suggested_users_skeleton_body`, trending topics + `getTrends` / `getTrendsSkeleton`, tagged suggestions, unspecced age-assurance state, suggestion / feed / starter-pack / onboarding / discover / explore / seeMore skeletons, `getPostThreadV2` / `getPostThreadOtherV2`, config |
| Labeler | `Labeler` | `app.bsky.labeler.getServices`; `policies_to_json` (`labelValues` / optional `labelValueDefinitions`) for `Records.labeler_service ~policies` |
| Chat / DMs | `Chat` | `chat.bsky.convo.*` including typed message facets/reactions/embeds, **system message data** (`addedBy` / `removedBy` / `approvedBy` / `unlockedBy` / `lockedBy`), `getConvoMembers` (`role` / `addedBy` / `chatDisabled` / `kind` / leftover `profileViewBasic` avatar / associated / viewer / labels / createdAt / verification), `getMessages.relatedProfiles`, `replyTo` union (`messageView` / `deletedMessageView` / `messageBeforeUserJoinedGroupView`), group convo leftover fields (`createdAt` / `joinLink` / `joinRequestCount` / `memberLimit`), `listConvoRequests` `convoView` / `joinRequestConvoView` union, `getLog` message / relatedProfiles / member; `chat.bsky.group` create/add/remove/edit + join links / join requests / mutual groups; notification prefs; actor status / declaration / `chat.bsky.actor.exportAccountData` / delete; moderation views + `subscribeModEvents`; production hosted path: `atproto-proxy` from default `did:web:api.bsky.chat#bsky_chat`, session `#bsky_chat`, or `ATP_CHAT_DID`; query-pair helpers (`list_convos_body` / `get_convo_body` / `get_messages_body`) shared with service-auth `list_convos_service` / `get_convo_service` / `get_messages_service` / `send_message_service` on `api.bsky.chat` (`Chat.service_aud` / `ATP_CHAT_HOST`; DPoP cannot be proxied); `Oauth.default_chat_scope` / `Chat.scope_has_chat` |
| Ozone | `Ozone` | `tools.ozone.moderation.*` including typed event/subject unions (`modEventMuteReporter` / `ageAssurance*` / `accountEvent` / `scheduleTakedownEvent` / leftover `modEventView` `creatorHandle` / `subjectHandle` / `modTool`, leftover `subjectStatusView` mute/takedown/appeal/age-assurance fields) plus typed `emit_event` encode (`event_to_json` / `subject_to_json` / `mod_tool_to_json` / `emit_event_typed` / `emit_event_service_typed`; raw Yojson `emit_event` unchanged), typed `create_activity` encode (`report_activity_to_json` / `create_activity_typed` / `create_activity_typed_body`; raw Yojson `create_activity` unchanged), subjects/repos/records, timeline, typed `get_account_preferences` (`Actor.preference` list / `app.bsky.actor.defs#preferences`, including `interestsPref.updatedAt`), reporter stats, typed `schedule_action` encode (`schedule_action_typed` / `schedule_action_typed_body`; optional typed `mod_tool`; raw Yojson `schedule_action` unchanged), `list_scheduled_actions` Yojson body (`list_scheduled_actions_body`; `list_scheduled_actions` shares that body), `cancel_scheduled_actions` Yojson body (`cancel_scheduled_actions_body`; `cancel_scheduled_actions` shares that body), query_events / query_statuses query-pair helpers (`query_events_body` / `query_statuses_body`; `query_events` / `query_events_service` / `query_statuses` share those pairs), leftover query_reports / search_repos query-pair helpers (`query_reports_body` / `search_repos_body`; `query_reports` / `search_repos` share those pairs); plus communication templates (`create_template_body`; `update_template_body` shared with `update_template`; `delete_template_body` `{ "id" }`), sets (`upsert_set_body` / `add_set_values_body` / `delete_set_values_body`; callers share those bodies), settings (`upsert_option_body`; no invented `managerRole`), team (`update_member_body`; `update_member` shares that body), safelink (`query_safelink_rules_body`; `query_safelink_rules` shares that body), signature, verification, hosting history + `getConfig` (`appview` / `pds` / `blobDivert` / `chat` / `viewer.role` / `verifierDid`); `tools.ozone.queue.*` (list/create/update/delete, moderator assign + `assignmentView.moderator` + `assign_queue_moderator_body` / `unassign_queue_moderator_body`, `routeReports` + `route_reports_body`) and `tools.ozone.report.*` (query/get, activities, assignments, stats, close/reassign); password sessions send `atproto-proxy` through the PDS; OAuth uses `getServiceAuth` + `emit_event_service` / `emit_event_service_typed` / `query_events_service` / `get_config_service` on the Ozone host |
| Admin | `Admin` | `com.atproto.admin` subject status, account info (`inviteNote` / `invitedBy` / `threatSignatures`), invites, email |
| Repo writes | `Repo`, `Records` | `createRecord` / `putRecord` / `deleteRecord` / `applyWrites` bodies via `Client.post_json` (`post_repo_write` keeps `string`); Yojson `create_record_json` / `put_record_json` (`create_record_body` / `put_record_body`; string `create_record` / `put_record` unchanged); `delete_record_body` + `apply_writes_parsed` (string `delete_record` / `apply_writes` unchanged); `blob_ref_to_json` (`parse_blob_ref` / `upload_blob` unchanged); string `describe_repo` / `get_record` / `list_records` via `Client.get_text` sharing `describe_repo_body` / `get_record_body` / `list_records_body` with typed parsers; `list_missing_blobs` via `Client.get_json` (`list_missing_blobs_body`); binary `upload_blob` / `import_repo` stay Cohttp; builders for post/like/repost/follow/block/listblock/list/listitem/starterpack/`referencelistoptout`/profile/status/contentVisibility/verification/threadgate/postgate/generator/labeler/notification declaration / `com.atproto.lexicon.schema` |
| Server | `Server` | session `describe_server` / `list_app_passwords` / `get_account_invite_codes` via `Client.get_text` (`get_account_invite_codes_body`; empty query shared with `describe_server_parsed`); `create_account` via `Client.post_json` sharing `create_account_body`; `create_invite_codes` shares `create_invite_codes_body`; describe server (typed), app passwords (`privileged` + typed `#appPassword` parse), invites (`createInviteCode` `forAccount`, `createInviteCodes` `forAccounts`), `reserveSigningKey`, account activate/status (`activateAccount` / `deactivateAccount` / typed `checkAccountStatus` including `repoCommit` / `repoRev` / `repoBlocks`), `createAccount` extras (`did`, `verificationCode` / `verificationPhone`, `plcOp`), `getServiceAuth` via `Client.get_json` (`get_service_auth_body`; aud may be `did#service`), email confirm/update (`confirmEmail`, `requestEmailConfirmation`, `requestEmailUpdate`, `updateEmail`). Procedures (`createInviteCode(s)`, `revokeAppPassword`, `resetPassword`, `deleteAccount`, `requestPasswordReset`, `requestAccountDelete`) POST JSON bodies per lexicon (they previously used GET) |
| Identity | `Identity`, `Did_plc`, `Did_web`, `Did_key` | resolve + typed `resolveDid` / `resolveIdentity` (`#identityInfo`) via `Client.get_json` (`resolve_handle` / `resolve_did` / `resolve_identity` share `resolve_handle_body` / `resolve_did_body` / `resolve_identity_query`). When the host returns `MethodNotImplemented` (PDS 0.5.x and current entryway), the client falls back to PLC (`PLC_ORIGIN` or `http://localhost:2582` on a local PDS) / `did:web` and wraps `{ didDoc }` |
| PLC chain | `Did_plc` | Genesis DID, prev CID links, p256 **and k256** ECDSA (low-S, IEEE P1363). Directory URLs accept a host or a full origin (`http://localhost:2582`); `PLC_ORIGIN` overrides the default `https://plc.directory`. Create/update/tombstone operation builders, directory `POST /{did}`, `GET /{did}/data`, and `GET /{did}/log/audit` |
| Sync | `Sync` | `getLatestCommit`, `getRepo` (CAR), public `getBlocks` (bytes/CAR), `listBlobs`, `listRepos`, host/repo status |
| CID / CAR | `Cid`, `Car`, `Dag_cbor` | CIDv1 (including SHA-256 `Cid.create`) + CARv1, blessed CID check, Sync 1.1 streamable pre-order, IPLD JSON ↔ DAG-CBOR (`of_yojson` / `to_yojson`; `$link` / `$bytes`) |
| MST | `Mst` | Layer/prefix rules, node parse, CID verify, lookup, insert/delete/walk, firehose-diff inversion **and** forward apply, `diff_ops` (prev tree → next tree), p256/k256 commit sign+verify, pre-order blocks, collection-range proofs |
| Repo sync (TAP-like) | `Repo_sync` | Library-ready indexer / backfill (not a hosted Tap): open/verify repo CAR, walk records as IPLD JSON (`walk` / `walk_json` / `record_json`), `getRecord` inclusion proof (`export_record_proof` / `verify_record_proof` / partial CAR), record-table apply of firehose ops, `#sync` desync, MST-level `apply_commit_tree`, Sync 1.1 pre-order export + collection-subset CAR, offline `write_signed_repo` (JSON → DAG-CBOR → MST → signed commit → CAR). `examples/repo_sync_indexer.ml` |
| TID | `Tid` | Record-key / commit-rev identifiers (base32-sortable, official syntax) |
| AT URI | `At_uri` | `at://` parse / serialize |
| Lexicon | `Lexicon` | Parse lexicon-1 JSON (parameters + procedure input/output schemas + `permission-set`), `to_ocaml` codegen (unions emit polymorphic variants), JSON validate, `resolveLexicon` client, small bundled official lexicon documents including `app.bsky.graph.referencelistoptout` and official OAuth permission-sets |
| Temp | `Temp` | `com.atproto.temp.checkHandleAvailability` (available / suggestions union), `checkSignupQueue`, `dereferenceScope`, plus privileged `addReservedHandle` / `requestPhoneVerification` / `revokeAccountCredentials` clients (no invented operator session). Deprecated `fetchLabels` remains `Label.query_labels` |
| Firehose | `Firehose`, `Websocket` | RFC 6455 client (`wss://` and local `ws://`) + `subscribeRepos` frame decode (`#commit`/`#sync`/`#identity`/`#account`/`#info`) |
| OAuth / DPoP | `Oauth`, `Oauth_scope` | PKCE S256, DPoP ES256 + nonce (RFC 9449 `htu` without query/fragment, random `jti`, RFC 7638 `dpop_jkt`), client metadata (`logo_uri` / `tos_uri` / `policy_uri`), **public HTTPS client-metadata** (`https_client_id` / `public_https_metadata` / `validate_https_metadata` / `metadata_document` / `metadata_http_response` / `fetch_client_metadata`; HTTP 200 + `application/json`; `client_id` in the document must match the fetch URL), production browser login glue (`start_browser_login` / `complete_browser_login`: discover → PAR → authorize URL → parse redirect → token exchange; no hosted login UI), PAR (`prompt=create` signup) / token / refresh / RFC 7009 revoke, `require_request_uri_registration`, resource-server `use_dpop_nonce` retry, `expect_sub` / `expires_at`; origin-aware URLs + loopback HTTP issuer; live Cohttp GET/POST (DPoP-Nonce + cookies); oauth-provider `~api` sign-in/consent helpers (real authorize CSRF/device cookies, no invented CSRF); DPoP XRPC (`xrpc_url` / `get_json_dpop` / `xrpc_post_dpop` / `get_service_auth`) so a client mints AppView, Ozone, or hosted-chat service-auth from the OAuth token and sends it with `Client.get_json ~bearer` / `Ozone.emit_event_service_typed` / `Chat.list_convos_service`; DPoP cannot be proxied; `default_chat_scope` / `Oauth_scope.has_chat` / `full_chat_client_scope` for DM grants (`transition:chat.bsky` or `include:chat.bsky.authFullChatClient`; `default_scope` is not enough); local TestNetwork discovery / hosted loopback metadata / PAR / token / getSession / getServiceAuth / AppView getTimeline / Ozone emitEvent / refresh / revoke; granular scope grammar (`repo:`/`rpc:`/`blob:`/`include:`/`transition:`) + official `app.bsky.auth*` / `chat.bsky.authFullChatClient` permission-set parse/expand |
| Labels | `Label` | `queryLabels` via `Client.get_json` (`query_labels` / `query_labels_parsed` share `query_labels_body`) + label / query parse (`ver`, `exp`) + `#selfLabels` + typed `#labelValueDefinition` parse/encode (`label_value_definition_strings_to_json` / `label_value_definition_to_json`; `severity` / `blurs` / `locales`; optional `defaultSetting` / `adultOnly`); `subscribeLabels` unchanged |
| XRPC headers | `Xrpc` | `atproto-proxy`, accept-labelers, rate-limit, `x-atproto-bsky-topics` (deprecated `x-bsky-topics`); service-auth JWT mint/verify (ES256/ES256K, `kid`/`jti`/`iat`/`lxm`, `did#service` aud, replay cache) |
| Errors | `Error` | XRPC `{error, message}` including rate limits |
| Syntax | `Syntax` | Handle, DID, NSID, record-key, datetime, language validators |
| Drafts | `Draft` | `app.bsky.draft` create/get/update/delete + typed draft / embed / threadgate / postgate builders plus typed create/update encode (`draft_to_json` / `create_draft_typed` / `update_draft_typed` / `*_typed_body`; raw Yojson `create_draft` / `update_draft` unchanged) |
| Contacts | `Contact` | `app.bsky.contact` phone verify, import, matches, dismiss, sync status, remove data; leftover `get_matches_body` (`limit` / `cursor`; `get_matches` / `get_matches_appview` / `get_matches_service` share those pairs); AppView service-auth `get_matches_appview` / `get_sync_status_appview`; OAuth `*_service` on AppView host (`import_contacts_service` / `start_phone_verification_service` / `verify_phone_service` / …); `ATP_PHONE` / `ATP_PHONE_NUMBER` skip gates. Client only — no SMS gateway |
| Age assurance | `Ageassurance` | `app.bsky.ageassurance` begin / getConfig / getState + region-rule union; stash `#event` parses `initIp` / `initUa` / `completeIp` / `completeUa` |
| Embeds / facets | `Embed`, `Facet` | Images, external (`readingTime`, `associatedProfiles`, source theme RGB, `associatedRefs`), record, recordWithMedia, video (`presentation` `default`/`gif`), **gallery**, record `#view` union; `getEmbedExternalView`; mention / link / tag parse **and serialize** |
| Notifications | `Notification` | All `listNotifications` known reasons; `listNotifications` query-pair helper (`list_notifications_body`; `list_notifications` / `list_notifications_page` share those pairs; currently sent fields only: optional `reasons` / `priority` / `cursor` / `seenAt` / `limit`); leftover `list_activity_subscriptions_body` (`limit` / `cursor`); `getUnreadCount` via `Client.get_json`; `updateSeen` Yojson `update_seen_body` via `Client.post_json` (empty output stays `""`); `putPreferences` v1 `put_preferences_body` (`priority`) and `putActivitySubscription` `put_activity_subscription_body` (`subject` / `activitySubscription`) via `Client.post_json`; prefs-v2; `register_push_body` / `unregister_push_body` (`serviceDid` / `token` / `platform` knownValues `ios` / `android` / `web` / `appId`; optional `ageRestricted`); optional `atproto-proxy` (`effective_push_proxy` / `ATP_PUSH_DID` / `Xrpc.notif_proxy`); `ATP_PUSH` skip gate. Client only — no APNs/FCM |
| User reports | `Moderation` | `com.atproto.moderation.createReport` (strongRef / repoRef, optional `modTool`, reason-type constants; Yojson `create_report_body_from_strong_ref` / `create_report_body_from_repo_ref`; string `create_report_data_from_*` unchanged) |
| Crypto / codecs | `K256`, `Base32`, `Base58`, `Base64url`, `Hash`, `Varint`, `Lt_hash` | secp256k1, multibase, CID/CAR varints; experimental proposal-0016 LtHash (not a spaces API) |
| HTTP helpers | `App`, `Client`, `Cohttp_client`, `Http_client`, `Http_method`, `Request`, `Response`, `User` | Endpoint URLs, shared XRPC GET/POST (Cohttp) + AppView `post_json_appview` service-auth (password `at+jwt`, or OAuth DPoP `Oauth.get_service_auth` + `get_json ~bearer`) + Ozone host/DID env (`ozone_host_from_env` / `ozone_did_from_env`), **HTTP/2 TLS** GET/POST/PUT/DELETE/PATCH via `Http_client` (IPv6 + `Client.get_json_h2` / `Client.post_json_h2` for public HTTPS). Requires HTTPS + ALPN `h2` |
| Sites | `Site` | Official `site.standard` records: document, publication, theme.basic/color, graph recommend + subscription plus typed record encodes (`document_to_json` / `publication_to_json` / `recommend_to_json` / `subscription_to_json`; siblings of `theme_to_json` / `contributor_to_json` / `parse_*`; lexicon fields only) |
| Germ Network | `Germnetwork` | `com.germnetwork.declaration` record (`$bytes` keys, `messageMe` policy) |

## Production OAuth (HTTPS client-metadata)

AT Protocol identifies a public client by an HTTPS `client_id` that **is** the URL of a JSON metadata document. This library builds, validates, and serializes that document and drives the browser login path. It does **not** host the file or a login UI.

1. Build the document (`Oauth.public_https_metadata` / `Oauth.validate_https_metadata`). `client_id` must be `https://host/path` with no port; web `redirect_uris` must be HTTPS on the same origin. Native clients may also use `http://127.0.0.1` / `http://[::1]` or a reverse-domain custom scheme (`com.example.app:/callback`).
2. Publish it at that URL over HTTPS as HTTP 200 with `Content-Type: application/json` (`Oauth.metadata_document` / `Oauth.metadata_http_response`). The body's `client_id` must exactly match the fetch URL. See `examples/client-metadata.json` and the tiny server sketch in `examples/oauth_https_metadata.ml`.
3. Discover the PDS authorization server, PAR, and redirect the browser to the authorize URL: `Oauth.start_browser_login` (PKCE S256 + DPoP + `state`).
4. On the redirect (`?code=&state=&iss=`), `Oauth.complete_browser_login` checks `state` / `iss` and exchanges the code for a DPoP token. Authed AppView / Ozone still use `Oauth.get_service_auth`, not the DPoP access token.

Loopback `http://localhost?redirect_uri=…` and local TestNetwork stay the development path. `Auth.createSession` / `make_auth_token_request` stay Cohttp password sessions.

## Production chat (hosted `chat.bsky.*`)

This library is a **client** for Bluesky hosted chat. It does not start or stub a chat service. Official TestNetwork still has no OSS chat backend.

**Scopes.** `Oauth.default_scope` (`atproto transition:generic`) is not enough for DMs. A chat-capable public client should declare `Oauth.default_chat_scope` (`atproto transition:generic transition:chat.bsky`) on the HTTPS metadata document and the PAR request. The granular alternative is `Oauth_scope.full_chat_client_scope` (`atproto include:chat.bsky.authFullChatClient`). The bundled permission-set in this pin expands to `listConvos` / `sendMessage` plus `chat.bsky.actor.declaration` — not every leftover `chat.bsky.*` NSID. Privileged app-passwords carry a chat/DM grant on the `createSession` JWT; regular app-passwords do not. `Chat.scope_has_chat` / `Oauth_scope.has_chat` detect `chat.bsky`, `bsky_chat`, or `authFullChatClient`. Live tests skip unless that predicate is true or `ATP_CHAT=1`.

**Proxy precedence** (`Chat.effective_proxy`): explicit `~proxy`, else the session DID-document `#bsky_chat` service (`did:web:<host>#bsky_chat`), else `ATP_CHAT_DID`, else `did:web:api.bsky.chat#bsky_chat`.

**Password session.** Privileged `createSession` + `Chat.list_convos` / `get_messages` / `send_message` through the PDS with `atproto-proxy`.

**OAuth / DPoP.** DPoP cannot be proxied (same rule as Ozone). After `Oauth.complete_browser_login`:

1. Mint `com.atproto.server.getServiceAuth` with DPoP (`Oauth.get_service_auth`; `aud` = `Chat.service_aud` / `did:web:api.bsky.chat`, `lxm` = the `chat.bsky.*` NSID). Password sessions can mint the same JWT with `Client.get_service_auth`.
2. Call `Chat.list_convos_service` / `get_convo_service` / `get_messages_service` / `send_message_service` on `Chat.default_host` (`api.bsky.chat` / `ATP_CHAT_HOST`) with that Bearer. Those helpers do **not** send `atproto-proxy`.

`examples/chat_production.ml` is that wiring as an offline sketch (scopes, proxy, query/POST bodies, `getServiceAuth` aud/lxm). It does not hit the network and is not a hosted chat product.

## Production video (hosted `app.bsky.video.*`)

This library is a **client** for Bluesky hosted video. It does not start or stub a transcoder. Official TestNetwork still has no video service.

**Service-auth.** `getServiceAuth` audience is `did:web:<pds-host>`, not `did:web:video.bsky.app`. Prefer the session DID document `#atproto_pds` endpoint (`Video.pds_audience`); entryway `ATP_HOST` is often `bsky.social`. `lxm` is `Video.upload_blob_lxm` (`com.atproto.repo.uploadBlob`), not `app.bsky.video.uploadVideo`. Recommended lifetime is `Video.recommended_exp` (1800s). Password sessions: `Video.mint_upload_token`. OAuth DPoP: `Oauth.get_service_auth` with the same aud / lxm / exp (DPoP cannot be sent as the video Bearer).

**Limits.** `Video.get_upload_limits_service` (and session `get_upload_limits`, which mints that JWT) call `app.bsky.video.getUploadLimits` on `Video.default_host` (`video.bsky.app` / `ATP_VIDEO_HOST`). Bluesky-hosted accounts typically need a verified email; daily remaining videos/bytes come back on that response. This is not a local quota server.

**Upload.** Small/typical clips: `Video.upload_video` (raw bytes, `Content-Type: video/mp4`) to `upload_video_url`. Larger files: multipart `start_upload` / `upload_part` / `finish_upload` (optional `abort_upload` / `get_upload_status`); `part_slice` gives offset/length when `total_bytes` is known. Both paths return a job id.

**Poll.** `Video.poll_job_status` / `ensure_blob` until a blob ref is present (`JOB_STATE_COMPLETED`, or `already_exists` with a blob — not a hard failure). Inject `get_status` / `sleep` in tests. Client poll only.

**Embed.** Put the job **blob ref** on `app.bsky.feed.post` via `Video.video_embed_json` / `embed_of_job` + `Records.post`. Optional `alt` / `aspect_ratio` / `presentation` (`default` / `gif`). The HLS **playlist** is `app.bsky.embed.video#view` after AppView hydrates the post — do not write the playlist into the create embed. This pin's `app.bsky.embed.video` create object has no captions field.

`examples/video_production.ml` is that wiring as an offline sketch (audience, upload URL, job/multipart helpers, embed → post). It does not hit the network and is not a hosted video product.

## Production indexer (TAP-like repo sync)

This library is a **client / backfill toolkit** for building an indexer. It does not start or stub a hosted Tap. Official TestNetwork is a local PDS + AppView + Ozone stack, not a Tap host.

**Backfill.** `Repo_sync.fetch_repo` / `backfill` pull `com.atproto.sync.getRepo`. Offline / tests use `open_car` / `open_car_bytes` / `resync_from_car` on a CAR you already have. `verify_snapshot` re-checks the MST; pass PLC / did:key `~keys` to check the commit signature.

**Walk.** `walk` is path + CID. `record_block` returns DAG-CBOR bytes. `record_json` / `walk_json` decode those bytes as IPLD JSON (`Dag_cbor.to_yojson`; `$link` / `$bytes`).

**getRecord proof.** `export_record_proof` builds a partial CAR (commit + MST covering path + record). `verify_record_proof` accepts that shape or a PDS `getRecord` CAR. Live: `fetch_record_proof`.

**Firehose.** While `Synchronized`, `process_commit` / `process_message` apply `#commit` ops to the in-memory record table (`live=true` for the tail). `apply_commit_tree` applies the same ops to the MST and checks `commit.data`. `#sync` with a different rev marks `Desynchronized` (`process_sync`); ignore further commits until `resync_from_car` / `backfill`.

**Sync 1.1 export.** `export_car` is a full pre-order repo CAR. `export_subset` / `verify_subset` are collection-range proofs.

**Offline fixture.** `write_signed_repo` is JSON → DAG-CBOR → MST → signed commit → CAR. The example signs with `Mst.encode_repo_commit` (unsigned). Production signers use `Mst.sign_p256` / `sign_k256`.

`examples/repo_sync_indexer.ml` is that wiring as an offline sketch (in-memory fixture CAR, walk, proof, firehose apply, `#sync` desync, Sync 1.1 export). It does not hit the network and is not a hosted Tap.

## Production Jetstream archive (operator API key)

This library is a **client** for Jetstream live tail and Network Replay HTTP. It does not invent an archive API key and does not pretend unauthenticated download works on Bluesky-hosted instances that require one.

**Live tail.** `subscribe` / `subscribe_one` stay unauthenticated and are not metered. v2 offers `Sec-WebSocket-Protocol: xrpc.v1.json`. Dict-zstd is `~compress:true`.

**Archive HTTP.** Bluesky-hosted `planSnapshot` / `planBackfill` / `listSegments` / `getSegment` / `getBlock` need an operator API key from [bsky.network/account](https://bsky.network/account). That is not a PDS session JWT and not `getServiceAuth`. Official TypeScript / Go SDKs read `JETSTREAM_API_KEY` and send `Authorization: Bearer`. This client does the same: explicit `~token`, else `JETSTREAM_API_KEY`, else `JETSTREAM_ARCHIVE_TOKEN` (alias). Pass the raw key — do not include a `Bearer ` prefix. Self-hosted Jetstream needs no key.

**Failing closed.** `require_archive_token` / `try_* ~require_token:true` raise `Archive_token_required` before HTTP when no key is available. Default `try_*` stay unauthenticated so public / self-hosted probes and CI remain skippable (`Snapshot_gated` 401/403). A missing, malformed, or revoked key returns 401 `{"error":"invalid bearer credential"}`. This library never fabricates a key.

**Resume.** `range_header` is the HTTP `Range` for `getSegment` after a mid-download 429 (`{"error":"byte limit exceeded"}`). Usage is metered in response bytes, not requests.

`examples/jetstream_archive.ml` is that wiring as an offline sketch (env names, Bearer header, official-over-alias precedence, `require_archive_token`). It injects a fixture string — not a real credential — and does not hit the network.

## Production phone / contacts / push (hosted AppView + caller gateway)

This library is a **client** for hosted Bluesky phone verification, contact import, and push registration. It does not send SMS and does not start an APNs/FCM gateway. Official TestNetwork still has no phone or push service.

**Contacts.** `app.bsky.contact.*` is AppView. Password sessions: `Contact.get_matches` / `get_sync_status` / `import_contacts` through the PDS / entryway. OAuth DPoP cannot be the AppView Bearer — mint `com.atproto.server.getServiceAuth` (`aud` = AppView DID, `lxm` = the `app.bsky.contact.*` NSID) and call `get_matches_service` / `get_sync_status_service` / `import_contacts_service` / `start_phone_verification_service` / `verify_phone_service` on `Client.appview_host_from_env` (`public.api.bsky.app` / `ATP_APPVIEW_HOST`). Query pairs are `get_matches_body` (`limit` / `cursor`).

**Hosted SMS.** `Contact.start_phone_verification` → `verify_phone` (returns `token`) → `import_contacts`. That is Bluesky-hosted SMS, not a local gateway. Live hops stay skippable unless `ATP_PHONE=1` and `ATP_PHONE_NUMBER` is a real E.164 number you own. `com.atproto.temp.requestPhoneVerification` is a different privileged signup-SMS client (`Temp.request_phone_verification`) and is also not faked. `Server.describe_server` reports `phoneVerificationRequired`; `createAccount` accepts `verificationPhone` / `verificationCode`.

**Push.** `Notification.register_push` / `unregister_push` take a caller-supplied `serviceDid` + device token + `platform` (`ios` / `android` / `web`) + `appId`. Official Bluesky push is closed to the official app. `Xrpc.notif_proxy` (`did:web:api.bsky.app#bsky_notif`) is the public service fragment, not a default credential. Optional `atproto-proxy` comes from `~proxy` or `ATP_PUSH_DID` (`effective_push_proxy`) — some PDS builds do not route `unregisterPush` to `#bsky_notif` themselves. Live hops stay skippable unless `ATP_PUSH=1` plus `ATP_PUSH_DID` / `ATP_PUSH_TOKEN` / `ATP_PUSH_APP_ID`.

`examples/contacts_production.ml` is that wiring as an offline sketch (query/POST bodies, AppView `*_service` helpers, push platforms, `notif_proxy`). It does not hit the network and is not an SMS or push product.

## Remaining gaps

These are product-level, not missing protocol cores.

**What 1.0.0 covers.** See [CHANGELOG.md](CHANGELOG.md) (`## [1.0.0]`) for the packaged client since tagged 0.1.0: OCaml 5 / packaging ([#229](https://github.com/david-engelmann/atproto/pull/229)), public HTTPS OAuth ([#230](https://github.com/david-engelmann/atproto/pull/230)), hosted chat ([#231](https://github.com/david-engelmann/atproto/pull/231)), hosted video ([#232](https://github.com/david-engelmann/atproto/pull/232)), TAP-like `Repo_sync` indexer ([#233](https://github.com/david-engelmann/atproto/pull/233)), Jetstream archive token env ([#234](https://github.com/david-engelmann/atproto/pull/234)), phone/contacts/push clients ([#235](https://github.com/david-engelmann/atproto/pull/235)), and the human-readable CHANGELOG ([#236](https://github.com/david-engelmann/atproto/pull/236)). Hosted-only SMS / APNs-FCM / unhosted feed generator stay listed not faked (`requestPhoneVerification` is not faked). Pin this GitHub repo for the 1.0.0 surface; tagging and the opam-repository 1.0.0 PR remain follow-ups.

- Official lexicons are pinned at bluesky-social/atproto [`f0d4877a03`](https://github.com/bluesky-social/atproto/commit/f0d4877a03dc8ede0d3e9a36d5b72ada63b5d2e0) (`app.bsky.actor.defs#interestsPref` `updatedAt`). `lexicons/official-nsids.json` is the compact NSID snapshot (`query` / `procedure` / `subscription` / `record` / `permission-set`); `scripts/gen-official-nsids.py` rebuilds it against a SHA. TestSuite `test_lexicon_coverage` fails if that pin grows and a public client NSID is missing a helper, record builder, bundled permission-set, or an explicit one-line skip. Hosted-only *servers* (no OSS chat backend, no video transcoder, no Tap host, no SMS gateway, no APNs/FCM push backend) are not skip reasons. Five deprecated/internal NSIDs are skipped in `lexicons/coverage-skips.json`: `com.atproto.temp.fetchLabels`, `com.atproto.sync.getCheckout`, `com.atproto.sync.getHead`, `com.atproto.sync.notifyOfUpdate`, and `internal.bsky.actor.getProfiles`.
- An application still has to **host** the HTTPS `client-metadata.json` (that URL is `client_id`) and receive the browser redirect against a remote PDS. The library now builds/validates the public HTTPS document and drives authorize → code → token (`start_browser_login` / `complete_browser_login`). It does not host the document or a login UI. Local TestNetwork already runs loopback metadata + PAR + DPoP through token, AppView service-auth, and Ozone privileged writes as `admin-mod.test`.
- A hosted **Tap** service. The indexer / backfill library path is documented and library-ready (`Repo_sync.open_car` / `verify_snapshot` / `walk_json` / `export_record_proof` / `verify_record_proof` / `process_commit` / `process_sync` / `apply_commit_tree` / Sync 1.1 `export_car` / `export_subset` / offline `write_signed_repo`; `examples/repo_sync_indexer.ml`). TestNetwork is a local PDS + AppView + Ozone stack, not a Tap host. This repo does not fake a hosted Tap.
- No hosted **video transcoder**. The production hosted client path is documented and library-ready (`Video.pds_audience` from `#atproto_pds`, `upload_blob_lxm` / `mint_upload_token` / `get_upload_limits_service` on `video.bsky.app`, injectable `poll_job_status`, multipart `part_slice`, `video_embed_json` / `embed_of_job`; `examples/video_production.ml`). Live video tests stay skippable unless `ATP_AUTH` is a real credential. This repo does not fake a local transcoder.
- No official **OSS chat** backend in `@atproto/dev-env` 0.6.4 TestNetwork (`ozone.chatUrl` = `localhost:2590`, “must run separate chat service”). The production hosted path is documented and library-ready (`Oauth.default_chat_scope` / `include:chat.bsky.authFullChatClient`, `Chat.effective_proxy` / `ATP_CHAT_DID`, service-auth `*_service` helpers on `api.bsky.chat` because DPoP cannot be proxied; `examples/chat_production.ml`). Live DM tests stay skippable unless `ATP_AUTH` has a chat/DM scope or `ATP_CHAT=1`. This repo does not fake a local chat service.
- Jetstream archive HTTP **download** on Bluesky-hosted instances still requires the **operator to supply** an API key (`JETSTREAM_API_KEY` / `JETSTREAM_ARCHIVE_TOKEN`, or `~token`). The library reads that env and sends `Authorization: Bearer`. It does not invent a key. Live `subscribeEvents` stays unauthenticated. Self-hosted archives may omit the key.
- No hosted **SMS / phone-verification** gateway. The production hosted client path is documented and library-ready (`Contact.get_matches_body` / `get_matches_appview` / `*_service`, `start_phone_verification` / `verify_phone` / `import_contacts`; `Temp.request_phone_verification`; `Server.describe_server` `phoneVerificationRequired`; `examples/contacts_production.ml`). Live SMS hops stay skippable unless `ATP_PHONE=1` and `ATP_PHONE_NUMBER` is a real number. Official TestNetwork does not start SMS. This repo does not fake `requestPhoneVerification`.
- No official **OSS push** gateway (official Bluesky push is closed to the official app; third-party clients host their own). The production hosted client path is documented and library-ready (`Notification.register_push_body` / `unregister_push_body` / `platform_ios` / `effective_push_proxy` / `Xrpc.notif_proxy`; `examples/contacts_production.ml`). Live register hops stay skippable unless `ATP_PUSH=1` plus a caller `serviceDid` / device token. This repo does not fake APNs/FCM.
- Permissioned data / spaces: experimental `Lt_hash` landed (proposal
  [0016](https://github.com/bluesky-social/proposals/blob/main/0016-permissioned-data/README.md)
  commit digest — 1024 little-endian uint16 lanes, unkeyed BLAKE3 XOF,
  lane-wise add/remove mod 2^16, commit `hash` = `sha256(state)`). The
  proposal is not final; this is not a stable spaces API. Space URI,
  signed commits (MAC/sig), credentials, XRPC, and sync stay deferred
  until 0016 stabilizes. This repo does not fake a space host.

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
let _ =
  Records.labeler_service
    ~policies:
      (Labeler.policies_to_json
         { label_values = [ "!hide" ]; label_value_definitions = [] })
    ~created_at:"2024-01-01T00:00:00.000Z" ()

(* MST layer for a repo key — official vector *)
let () = assert (Mst.layer_for_key "blue" = 1)

(* experimental proposal-0016 LtHash — not a spaces API *)
let () =
  assert (Lt_hash.is_empty (Lt_hash.empty ()));
  assert (
    Hash.hex_encode (Lt_hash.hash (Lt_hash.empty ()))
    = "e5a00aa9991ac8a5ee3109844d84a55583bd20572ad3ffcd42792f3c36b183ad")

(* TID used as record keys and commit revs *)
let () = assert (Tid.is_valid "3jzfcijpj2z2a")

(* typed Bluesky record builders + facet serialize *)
let post =
  Records.post ~text:"hello #atproto" ~created_at:"2024-01-01T00:00:00.000Z"
    ~facets:[ Facet.tag ~byte_start:6 ~byte_end:14 "atproto" ]
    ()

(* OAuth: publish HTTPS client-metadata, then drive authorize → code → token.
   The app still hosts the JSON at client_id and receives /cb. *)
let client_id = Oauth.https_client_id ~host:"client.example" ()
let meta =
  Oauth.public_https_metadata ~client_id
    ~redirect_uris:[ "https://client.example/cb" ] ()
let _ = Oauth.metadata_http_response meta
(* Official loopback client_id for local / TestNetwork development *)
let loopback =
  Oauth.loopback_client_id ~redirect_uri:"http://127.0.0.1:8080/cb" ()
let _ = Oauth.localhost_metadata loopback
(* Production browser path (inject live Cohttp, or a test double):
   let login = Oauth.start_browser_login ~http_get ~http_post ~priv ~pub
     ~pds_origin ~client_id ~redirect_uri () in
   (* redirect the user-agent to login.authorize_url *)
   let token, _ = Oauth.complete_browser_login ~http:http_post ~priv ~pub
     ~login ~redirect:callback_uri () *)

(* video: service-auth aud = did:web:<pds> from #atproto_pds, lxm = uploadBlob *)
let upload =
  Video.upload_video_url ~did:"did:plc:abc123xyz0001112223333" ~name:"clip.mp4" ()
let embed =
  Video.video_embed_json
    ~video:(`Assoc [ ("$type", `String "blob"); ("mimeType", `String "video/mp4") ])
    ~alt:"demo" ~presentation:"gif" ()
let _ = Video.get_job_status_body ~job_id:"job-1" ()
let _ = Video.upload_video_body ~did:"did:plc:abc123xyz0001112223333" ~name:"clip.mp4" ()
(* after mint_upload_token / Oauth.get_service_auth and poll_job_status:
   Records.post ~text ~created_at ~embed:(Video.embed_of_blob blob) () *)
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
(* archive HTTP: operator key from JETSTREAM_API_KEY (or ~token).
   Live subscribe stays unauthenticated. See examples/jetstream_archive.ml *)
let _auth = Jetstream.archive_authorization ~token:"operator-supplied-key" ()
let _ = Jetstream.require_archive_token ~token:"operator-supplied-key" ()

(* TAP-like local indexer: backfill a CAR, then apply firehose ops *)
let acct =
  Repo_sync.create_account ~did:"did:plc:abc123xyz0001112223333"
    ~collections:[ "app.bsky.feed.post" ] ()
let _ = Repo_sync.status_to_string acct.Repo_sync.status
(* after Sync.get_repo / open_car: walk_json, export_record_proof,
   process_commit, process_sync → resync_from_car. See
   examples/repo_sync_indexer.ml (offline, no hosted Tap). *)

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
let _ = Graph.unmute_actor_body ~actor:"alice.test"
let _ =
  Graph.mute_actor_list_body
    ~list:"at://did:plc:abc123xyz0001112223333/app.bsky.graph.list/3k2a"
let _ =
  Graph.mute_thread_body
    ~root:"at://did:plc:abc123xyz0001112223333/app.bsky.feed.post/3k2b"
let _ =
  Graph.get_list_body
    ~list:"at://did:plc:abc123xyz0001112223333/app.bsky.graph.list/3k2a"
    ~limit:10 ()
let _ = Graph.search_starter_packs_body ~q:"bluesky" ~limit:5 ()
let _ =
  Auth.create_session_body ~identifier:"alice.test" ~password:"x"
    ~allow_takendown:true ()
let bookmarks = Bookmark.get_bookmarks session ~limit:10 ()
(* chat: password + atproto-proxy, or OAuth service-auth on api.bsky.chat *)
let _ = Oauth.default_chat_scope
let convos = Chat.list_convos session ~limit:10 ()
let chat_status = Chat.get_actor_status session ()
let _ = Chat.effective_proxy ~did_doc:(Option.value ~default:`Null session.did_doc) ()
let _ = Chat.list_convos_body ~limit:10 ()
let _ = Chat.service_aud ()
(* after Oauth.get_service_auth ~aud:(Chat.service_aud ()) ~lxm:"chat.bsky.convo.listConvos":
   Chat.list_convos_service ~bearer ~host:Chat.default_host ~limit:10 () *)

(* site.standard + germnetwork records — local builders, no network *)
let article =
  Site.document ~site:"https://standard.site" ~title:"hello"
    ~published_at:"2026-01-01T00:00:00.000Z" ()
let _ = Site.document_to_json (Site.parse_document article)
let germ = Germnetwork.declaration ~version:"1.0.0" ~current_key:"key" ()

(* topics header for Client.get_json ~extra — current x-atproto-bsky-topics *)
let _topics = Xrpc.topics_headers [ "news"; "sports" ]

(* HTTP/2 XRPC GET that keeps status + rate-limit headers (skips if ALPN h2 fails) *)
let _h2 =
  Http_client.xrpc_url ~host:"public.api.bsky.app"
    "com.atproto.identity.resolveHandle" ~query:[ ("handle", "bsky.app") ] ()
```
