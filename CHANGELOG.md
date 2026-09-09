# Changelog

Notes for the packaged **0.1.0** library. This file records what actually
shipped through public HTTPS OAuth client-metadata + production
browser-login path (`https_client_id` / `public_https_metadata` /
`validate_https_metadata` / `metadata_document` /
`metadata_http_response` / `fetch_client_metadata` /
`start_browser_login` / `complete_browser_login`; the application
still hosts the HTTPS document and redirect URI; no hosted login UI)
on top of [#229](https://github.com/david-engelmann/atproto/pull/229):
OCaml 5 / packaging modernization hop toward 1.0.0
(dune lang 3.11, OCaml `>= 4.14.1` and `< 5.4`, Jane Street
`core` / `async` / `ppx_jane` / `zstandard` `>= v0.16.0` and
`< v0.18~`, CI `build` on 4.14.1 + 5.3.0; package version stays
`0.1.0` and is not retagged)
on top of
[#228](https://github.com/david-engelmann/atproto/pull/228):
opam-repository publish prep (package description / README / CONTRIBUTING /
odoc landing say the package is published on opam-repository;
`opam install atproto` after the opam-repository PR merges; GitHub pin
still works for development)
on top of
[#227](https://github.com/david-engelmann/atproto/pull/227):
Feed leftover getAuthorFeed / getFeedSkeleton query-pair helpers
(`get_author_feed_body` / `get_feed_skeleton_body`;
`get_author_feed` / `get_author_feed_page` / `get_feed_skeleton` /
`get_feed_skeleton_parsed` share those pairs)
on top of
[#226](https://github.com/david-engelmann/atproto/pull/226):
Notification leftover list_notifications query-pair helpers
(`list_notifications_body`;
`list_notifications` / `list_notifications_page` share those pairs)
on top of
[#225](https://github.com/david-engelmann/atproto/pull/225):
Ozone leftover query_reports / search_repos query-pair helpers
(`query_reports_body` / `search_repos_body`;
`query_reports` / `search_repos` share those pairs)
on top of
[#224](https://github.com/david-engelmann/atproto/pull/224):
Unspecced leftover getSuggestionsSkeleton / getPostThreadV2 /
suggested-users query-pair helpers
(`get_suggestions_skeleton_body` / `get_post_thread_v2_body` /
`get_suggested_users_body` / `get_suggested_users_skeleton_body`;
`get_suggestions_skeleton` / `get_post_thread_v2` /
`get_suggested_users` / `get_suggested_onboarding_users` /
`get_suggested_users_for_explore` / `get_suggested_users_for_see_more` /
`get_suggested_users_skeleton` /
`get_onboarding_suggested_users_skeleton` /
`get_suggested_users_for_explore_skeleton` /
`get_suggested_users_for_see_more_skeleton` share those pairs)
on top of
[#223](https://github.com/david-engelmann/atproto/pull/223):
Unspecced leftover searchPostsSkeleton / searchActorsSkeleton /
searchStarterPacksSkeleton query-pair helpers
(`search_posts_skeleton_body` / `search_actors_skeleton_body` /
`search_starter_packs_skeleton_body`;
`search_posts_skeleton` / `search_actors_skeleton` /
`search_starter_packs_skeleton` share those pairs)
on top of
[#222](https://github.com/david-engelmann/atproto/pull/222):
Feed leftover searchPostsV2 / getQuotes / getActorLikes query-pair helpers
(`search_posts_v2_body` / `get_quotes_body` / `get_actor_likes_body`;
`search_posts_v2` / `get_quotes` / `get_actor_likes` share those pairs)
on top of
[#221](https://github.com/david-engelmann/atproto/pull/221):
Graph leftover AppView query-pair helpers
(`get_list_body` / `get_lists_body` / `get_actor_starter_packs_body` /
`search_starter_packs_body` / `get_relationships_body` /
`get_known_followers_body`; `get_list` / `get_lists` /
`get_actor_starter_packs` / `search_starter_packs` /
`search_starter_packs_v2` / `get_relationships` /
`get_known_followers` share those pairs) on top of
[#220](https://github.com/david-engelmann/atproto/pull/220):
Feed leftover AppView query-pair helpers
(`get_feed_body` / `get_list_feed_body` / `get_actor_feeds_body` /
`search_posts_body`; `get_feed` / `get_list_feed` / `get_actor_feeds` /
`search_posts` share those pairs) on top of
[#219](https://github.com/david-engelmann/atproto/pull/219):
Ozone `query_events` / `query_statuses` query-pair helpers
(`query_events_body` / `query_statuses_body`; `query_events` /
`query_events_service` / `query_statuses` share those pairs) on top of
[#216](https://github.com/david-engelmann/atproto/pull/216):
Ozone queue leftover Yojson POST body helpers
(`assign_queue_moderator_body` / `unassign_queue_moderator_body` /
`route_reports_body`; `assign_queue_moderator` /
`unassign_queue_moderator` / `route_reports` share those bodies;
`tools.ozone.queue.assignModerator` / `unassignModerator` /
`routeReports` only — not `report.assignModerator`) on top of
[#215](https://github.com/david-engelmann/atproto/pull/215):
Ozone set/setting/team leftover Yojson POST body helpers
(`upsert_set_body` / `add_set_values_body` / `delete_set_values_body` /
`upsert_option_body` / `update_member_body`;
`upsert_set` / `add_set_values` / `delete_set_values` / `upsert_option` /
`update_member` share those bodies) on top of
[#218](https://github.com/david-engelmann/atproto/pull/218): Site typed record encodes
(`document_to_json` / `publication_to_json` / `recommend_to_json` /
`subscription_to_json`; siblings of `theme_to_json` /
`contributor_to_json` / `parse_*`; lexicon fields only) on top of
[#214](https://github.com/david-engelmann/atproto/pull/214):
Ozone report leftover Yojson POST body helpers
(`assign_report_moderator_body` / `reassign_queue_body` /
`refresh_stats_body` / `close_reports_body`;
`assign_report_moderator` / `reassign_queue` / `refresh_stats` /
`close_reports` share those bodies) on top of
[#217](https://github.com/david-engelmann/atproto/pull/217): Label `label_value_definition` encode helpers
(`label_value_definition_strings_to_json` /
`label_value_definition_to_json`; required `identifier` / `severity` /
`blurs` / `locales`, optional `defaultSetting` / `adultOnly`) plus
Labeler `policies_to_json` (`labelValues` / optional
`labelValueDefinitions`) so `Records.labeler_service ~policies` has a
typed path; parse types and `labeler_service` Yojson signature
unchanged; does not invent leftover definition fields on top of
[#213](https://github.com/david-engelmann/atproto/pull/213):
Ozone `query_safelink_rules` Yojson body helper
(`query_safelink_rules_body`; `query_safelink_rules` shares that
body) on top of
[#212](https://github.com/david-engelmann/atproto/pull/212):
Ozone communication `update_template` Yojson body helper
(`update_template_body`; `update_template` shares that body; current
params only: required `id`, optional `name` / `contentMarkdown` /
`subject` / `updatedBy` / `disabled`) plus `delete_template_body`
(`{ "id" }`) on top of
[#211](https://github.com/david-engelmann/atproto/pull/211):
Ozone `list_scheduled_actions` Yojson body helper
(`list_scheduled_actions_body`; `list_scheduled_actions` shares that
body) on top of
[#210](https://github.com/david-engelmann/atproto/pull/210):
Notification putPreferences v1 + putActivitySubscription Yojson body
helpers (`put_preferences_body` / `put_activity_subscription_body`;
`put_preferences` / `put_activity_subscription` share those bodies via
`Client.post_json`; public signatures unchanged) on top of
[#209](https://github.com/david-engelmann/atproto/pull/209):
Graph muteActorList / muteThread Yojson body helpers
(`mute_actor_list_body` / `unmute_actor_list_body` /
`mute_thread_body` / `unmute_thread_body`; `mute_actor_list` /
`unmute_actor_list` / `mute_thread` / `unmute_thread` share those
bodies via `Client.post_json`; public signatures unchanged) on top of
[#208](https://github.com/david-engelmann/atproto/pull/208):
Server getServiceAuth via Client
(`get_service_auth` via `Client.get_json` sharing `get_service_auth_body`
with `get_service_auth_url`; reuses `Xrpc.service_auth_body` for aud /
lxm validation; `service_auth` parse type and public signature
unchanged) on top of
[#207](https://github.com/david-engelmann/atproto/pull/207):
Sync leftover JSON XRPC via Client
(`get_latest_commit` / `get_repo_status` / `list_repos` / `list_blobs`
/ `list_hosts` / `get_host_status` / `list_repos_by_collection` share
`get_latest_commit_body` / `get_repo_status_body` / `list_repos_body`
/ `list_blobs_body` / `list_hosts_body` / `get_host_status_body` /
`list_repos_by_collection_body`; `request_crawl` via
`Client.post_json` with `request_crawl_body`, empty procedure output
stays `""`; binary `getRepo` / `getBlob` / `getBlocks` / `getRecord`
CAR stay Cohttp; public signatures and parse types unchanged) on top of
[#206](https://github.com/david-engelmann/atproto/pull/206):
Identity resolveHandle/resolveDid via Client
(`resolve_handle` / `resolve_did` / `resolve_identity` share
`resolve_handle_body` / `resolve_did_body` /
`resolve_identity_query`; parsed return types unchanged) on top of
[#205](https://github.com/david-engelmann/atproto/pull/205):
Session leftover JSON XRPC via Client
(`get_session_request` via `Client.get_text`; `refresh_session` /
`delete_session` via `Client.post_json` with Bearer `refreshJwt`,
empty delete output stays `""`) on top of
[#204](https://github.com/david-engelmann/atproto/pull/204):
Server leftover session JSON XRPC via Client
(`describe_server` / `create_account` / `list_app_passwords` /
`get_account_invite_codes` share `create_account_body` /
`get_account_invite_codes_body` / `create_invite_codes_body`;
`describe_server` / `list_app_passwords` / `get_account_invite_codes`
via `Client.get_text`; `create_account` via `Client.post_json`;
public signatures and parse types unchanged) on top of
[#203](https://github.com/david-engelmann/atproto/pull/203):
Notification leftover JSON XRPC via Client
(`get_unread_count` via `Client.get_json`; `update_seen_body` plus
`update_seen` via `Client.post_json`, empty procedure output stays
`""`) on top of
[#202](https://github.com/david-engelmann/atproto/pull/202):
Feed leftover session JSON XRPC via Client
(`get_author_feed` / `get_likes` / `get_post_thread` / `get_posts` /
`get_reposted_by` / `get_timeline` / `get_feed_skeleton` share
`get_author_feed_body` / `get_likes_body` / `get_post_thread_body` /
`get_posts_body` / `get_reposted_by_body` / `get_timeline_body` /
`get_feed_skeleton_body`; parsed return types unchanged;
`get_feed_skeleton` via `Client.get_text`) on top of
[#201](https://github.com/david-engelmann/atproto/pull/201):
Repo leftover JSON XRPC via Client
(`describe_repo` / `get_record` / `list_records` share
`describe_repo_body` / `get_record_body` / `list_records_body` with
parsed helpers; `post_repo_write` via `Client.post_json`;
`list_missing_blobs` shares `list_missing_blobs_body`; binary
`upload_blob` / `import_repo` stay Cohttp) on top of
[#200](https://github.com/david-engelmann/atproto/pull/200):
Actor profile/search/suggestions via `Client.get_json`
(`get_profile` / `get_profiles` / `get_suggestions` / `search_actors` /
`search_actors_typeahead` share `get_profile_body` /
`get_profiles_body` / `get_suggestions_body` / `search_actors_body`;
parsed return types unchanged) on top of
[#199](https://github.com/david-engelmann/atproto/pull/199):
Label `queryLabels` via `Client.get_json`
(`query_labels` / `query_labels_parsed` share `query_labels_body`;
`subscribeLabels` unchanged) on top of
[#197](https://github.com/david-engelmann/atproto/pull/197):
Ozone `cancel_scheduled_actions` Yojson body helper
(`cancel_scheduled_actions_body`; `cancel_scheduled_actions` shares that
body) on top of
[#198](https://github.com/david-engelmann/atproto/pull/198): Graph mute/unmute Client +
`unmute_actor_body` (`unmute_actor_body`; `mute_actor` /
`unmute_actor` via `Client.post_json`, string return unchanged; session
`get_blocks` / `get_mutes` / `get_follows` / `get_followers` via
`Client.get_json` with `limit`; parsed return types unchanged) on top of
[#196](https://github.com/david-engelmann/atproto/pull/196):
typed `Ozone.schedule_action` encoding
(`schedule_action_typed` / `schedule_action_typed_body`; optional typed
`mod_tool`; raw Yojson `schedule_action` unchanged) on top of
[#195](https://github.com/david-engelmann/atproto/pull/195):
Moderation `create_report` Yojson body helpers
(`create_report_body_from_strong_ref` / `create_report_body_from_repo_ref`;
string `create_report_data_from_strong_ref` /
`create_report_data_from_repo_ref` unchanged) on top of
[#194](https://github.com/david-engelmann/atproto/pull/194):
Repo `blob_ref_to_json` for upload→record
(`blob_ref_to_json`; `parse_blob_ref` / `upload_blob` unchanged) on top of
[#193](https://github.com/david-engelmann/atproto/pull/193):
Repo `delete_record_body` + parsed `apply_writes`
(`delete_record_body` / `apply_writes_parsed`; string `delete_record` /
`apply_writes` unchanged) on top of
[#192](https://github.com/david-engelmann/atproto/pull/192):
Repo create/put Yojson record helpers
(`create_record_json` / `put_record_json` / `create_record_body` /
`put_record_body`; string `create_record` / `put_record` unchanged) on top of
[#191](https://github.com/david-engelmann/atproto/pull/191):
typed `Ozone.create_activity` encoding
(`report_activity_to_json` / `create_activity_typed` /
`create_activity_typed_body`; raw Yojson `create_activity` unchanged)
on top of
[#190](https://github.com/david-engelmann/atproto/pull/190):
typed Draft create/update helpers
(`draft_to_json` / `create_draft_typed` / `update_draft_typed` /
`create_draft_typed_body` / `update_draft_typed_body`; raw Yojson
`create_draft` / `update_draft` unchanged) on top of
[#189](https://github.com/david-engelmann/atproto/pull/189):
Xrpc `x-atproto-bsky-topics` helpers
(`topics_to_string` / `topics_header` / `legacy_topics_header` /
`topics_headers` / `parse_topics` / `topics_from_headers`; current
header `x-atproto-bsky-topics`, deprecated `x-bsky-topics`; no lexicon
pin bump) on top of
[#188](https://github.com/david-engelmann/atproto/pull/188):
OAuth live Ozone hop uses `emit_event_service_typed` on top of
[#187](https://github.com/david-engelmann/atproto/pull/187):
typed `Ozone.emit_event` encoding
(`event_to_json` / `subject_to_json` / `mod_tool_to_json` /
`emit_event_typed` / `emit_event_service_typed`) on top of
[#186](https://github.com/david-engelmann/atproto/pull/186):
typed `Ozone.get_account_preferences` via `Actor.preferences` (same
16-variant `app.bsky.actor.defs#preferences` union, including
`interestsPref.updatedAt`) on top of
[#185](https://github.com/david-engelmann/atproto/pull/185):
typed Actor `putPreferences` encoding
(`preference_kind_to_json` / `preference_to_json` /
`preferences_to_json` / `put_preferences_typed`, including optional
`interestsPref.updatedAt`) on top of
[#184](https://github.com/david-engelmann/atproto/pull/184):
official lexicon pin `f0d4877a` and `Ozone.get_account_preferences`
([#132](https://github.com/david-engelmann/atproto/pull/132)), live leftover
TestNetwork NSIDs ([#129](https://github.com/david-engelmann/atproto/pull/129))
plus live `getAccountPreferences`
([#137](https://github.com/david-engelmann/atproto/pull/137)), live leftover
TestNetwork hops for remaining `com.atproto.admin` NSIDs
([#150](https://github.com/david-engelmann/atproto/pull/150)), leftover
`com.atproto.server` / AppView / ozone report hops
([#152](https://github.com/david-engelmann/atproto/pull/152)–[#154](https://github.com/david-engelmann/atproto/pull/154)),
leftover `com.atproto.temp` operator hops
([#162](https://github.com/david-engelmann/atproto/pull/162)), live leftover
AppView `getFeedSkeleton`
([#163](https://github.com/david-engelmann/atproto/pull/163)), live leftover
unspecced age-assurance
([#168](https://github.com/david-engelmann/atproto/pull/168)), odoc landing
leftover modules ([#165](https://github.com/david-engelmann/atproto/pull/165)),
remaining function-level odoc through
[#166](https://github.com/david-engelmann/atproto/pull/166), odoc landing
leftover public protocol modules
([#170](https://github.com/david-engelmann/atproto/pull/170)), compiled-only
`examples/offline.ml` constructor/parser coverage
([#171](https://github.com/david-engelmann/atproto/pull/171)),
installed-package consumer smoke
([#172](https://github.com/david-engelmann/atproto/pull/172)), notes through
[#172](https://github.com/david-engelmann/atproto/pull/172)
([#173](https://github.com/david-engelmann/atproto/pull/173)), documented
libzstd system dependency for Jane Street `zstandard` / Jetstream
dict-zstd (Ubuntu/Debian `libzstd-dev`, Homebrew `zstd`)
([#174](https://github.com/david-engelmann/atproto/pull/174)), notes through
[#174](https://github.com/david-engelmann/atproto/pull/174)
([#175](https://github.com/david-engelmann/atproto/pull/175)), Error on
the odoc landing map plus libzstd install notes in odoc / the install
issue template ([#176](https://github.com/david-engelmann/atproto/pull/176)),
remaining function-level odoc on `Client.get_json_h2` /
`Client.post_json_h2` plus `Feed.filter_posts_*`
([#177](https://github.com/david-engelmann/atproto/pull/177)), notes through
[#177](https://github.com/david-engelmann/atproto/pull/177)
([#178](https://github.com/david-engelmann/atproto/pull/178), `586b4877`),
merge-when-green squash automation (`95b6eb57`; workflow + script +
CONTRIBUTING; docs-only allowlist excludes `.github/scripts` and
workflows; green non-draft same-repo/Dependabot PRs to `main` may
auto-squash-merge, see CONTRIBUTING)
([#179](https://github.com/david-engelmann/atproto/pull/179)), notes
through [#179](https://github.com/david-engelmann/atproto/pull/179)
([#180](https://github.com/david-engelmann/atproto/pull/180), `82d8b543`),
lexicon pin `f0d4877a` for actor interests `updatedAt`
(`Actor.interests_pref` = `{ tags; updated_at : string option }`;
`6af3edf1`)
([#181](https://github.com/david-engelmann/atproto/pull/181)),
merge-when-green ignoring lexicon-pin drift (`84040dc0`; pin drift is
advisory and must not deadlock docs-only or full-mode merges)
([#182](https://github.com/david-engelmann/atproto/pull/182)), notes
through [#182](https://github.com/david-engelmann/atproto/pull/182)
([#183](https://github.com/david-engelmann/atproto/pull/183), `7fd95c86`),
and live leftover AppView / PDS `getPreferences` / `putPreferences`
hops that assert `interestsPref.updatedAt` when present
([#184](https://github.com/david-engelmann/atproto/pull/184)).

This package is published on the public
[opam-repository](https://github.com/ocaml/opam-repository)
(`opam install atproto`; pending merge of the opam-repository PR).
Pin the GitHub repository for development (see the README). Requires
OCaml `>= 4.14.1` and `< 5.4` (CI: 4.14.1 and 5.3.0). Jane Street
`core` / `async` / `ppx_jane` / `zstandard` are `>= v0.16.0` and
`< v0.18~` (v0.16 on 4.14, v0.17 on 5.1–5.3). System libzstd is
still required.

## 0.1.0 — 2026-09-03

First installable opam package (`dune-project` / `atproto.opam` version
`0.1.0`, OCaml `>= 4.14.1` and `< 5.0`). `opam install atproto` is the
published install once the opam-repository PR merges.
`opam pin add atproto git+https://github.com/david-engelmann/atproto.git`
exposes `(libraries atproto)` for development.

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
  RFC 7009 revoke, public HTTPS client-metadata helpers + production
  browser-login path (`start_browser_login` / `complete_browser_login`;
  the app still hosts the HTTPS document), granular scopes, official
  `app.bsky.auth*` / `chat.bsky.authFullChatClient` permission-sets
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
  `getServiceAuth` + `Ozone.emit_event_service_typed` (DPoP cannot be
  proxied)
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
- [#150](https://github.com/david-engelmann/atproto/pull/150): live leftover
  TestNetwork hops for remaining `com.atproto.admin` NSIDs
  (`getSubjectStatus` / `updateSubjectStatus`,
  `getAccountInfo` / `getAccountInfos` / `searchAccounts`,
  `enableAccountInvites` / `disableAccountInvites` / `getInviteCodes` /
  `disableInviteCodes`, `sendEmail`, `updateAccountEmail` /
  `updateAccountHandle` / `updateAccountPassword` /
  `updateAccountSigningKey`, throwaway `deleteAccount` — never
  `alice.test`). Skip when not served, MethodNotImplemented,
  feature-disabled, UpstreamFailure, email-token, or InvalidToken is
  TestNetwork policy. Does not fake a hosted admin panel
- [#152](https://github.com/david-engelmann/atproto/pull/152): live leftover
  TestNetwork hops for remaining `com.atproto.server` NSIDs
  (`createInviteCode` / `createInviteCodes`,
  `requestEmailConfirmation`, `confirmEmail`, `updateEmail`,
  `requestPasswordReset` / `resetPassword`). Skip when not served
  or TestNetwork policy (email-token / InvalidToken / SMTP).
  Throwaway account, never `alice.test`. Does not invent SMTP
- [#153](https://github.com/david-engelmann/atproto/pull/153): live leftover
  TestNetwork hops for AppView `app.bsky.feed.sendInteractions`,
  `describeFeedGenerator`, and notification `putPreferences` v1.
  Skip when the NSID is not served or TestNetwork policy. Hosted
  chat / video / Tap / push / contacts stay listed not faked
- [#154](https://github.com/david-engelmann/atproto/pull/154): live leftover
  TestNetwork hops for `tools.ozone.report.getAssignments` /
  `unassignModerator` (not the `queue.*` twins). Skip when not
  served or TestNetwork policy. Does not fake a hosted ozone
  report store
- [#162](https://github.com/david-engelmann/atproto/pull/162): live leftover
  TestNetwork hops for remaining `com.atproto.temp` operator NSIDs
  (`addReservedHandle` / `revokeAccountCredentials`). Skip when
  not served or TestNetwork policy. Throwaway account, never
  `alice.test`. `requestPhoneVerification` stays listed not faked
- [#163](https://github.com/david-engelmann/atproto/pull/163): live leftover
  TestNetwork hop for AppView `app.bsky.feed.getFeedSkeleton` only
  against our leftover generator when `is_online`. Skip unhosted
  generator / not served. Policy-skip, not fake
- [#168](https://github.com/david-engelmann/atproto/pull/168): live leftover
  TestNetwork hops for AppView `app.bsky.unspecced.getAgeAssuranceState` /
  `initAgeAssurance`. Skip when not served or TestNetwork policy.
  Distinct from the dedicated `app.bsky.ageassurance.*` hops. No hosted
  verifier faked
- [#184](https://github.com/david-engelmann/atproto/pull/184): live leftover
  TestNetwork hops for AppView and PDS
  `app.bsky.actor.getPreferences` / `putPreferences` that assert
  `interestsPref.updatedAt` when the served JSON includes it (pin
  `f0d4877a`). Skip if not served or TestNetwork policy. Parse +
  offline coverage shipped in
  [#181](https://github.com/david-engelmann/atproto/pull/181). Does
  not fake hosted chat / video / Tap / phone / contacts / push
- [#185](https://github.com/david-engelmann/atproto/pull/185): typed
  Actor `putPreferences` encoding: `preference_kind_to_json` /
  `preference_to_json` / `preferences_to_json` plus
  `put_preferences_typed` / `put_preferences_typed_body`. Serializes
  `preference_kind` including `Interests` with optional camelCase
  `updatedAt`. `Other` / unknown reuse `.original`. Raw Yojson
  `put_preferences` / `put_preferences_body` stay unchanged. Unit
  tests only; live leftover hops stay in
  [#184](https://github.com/david-engelmann/atproto/pull/184). No
  hosted chat / video / Tap / phone / contacts / push faked
- [#186](https://github.com/david-engelmann/atproto/pull/186): typed
  `Ozone.get_account_preferences` / `parse_account_preferences`.
  `account_preferences.preferences` is `Actor.preference list` (not
  `Yojson.Safe.t list`); each item is the same 16-variant union
  `Actor.parse_preference` already handles, including `Interests`
  `updated_at`. Live TestNetwork hop asserts typed kinds when present
  and `interestsPref.updatedAt` when the served JSON includes it.
  Compile-breaking type change; no backwards-compat. Does not fold
  leftover `emitEvent` fields or add `emit_event_typed`. No hosted
  chat / video / Tap / phone / contacts / push faked
- [#187](https://github.com/david-engelmann/atproto/pull/187): typed
  `Ozone.emit_event` encoding: `event_to_json` /
  `subject_to_json` / `mod_tool_to_json` plus
  `emit_event_typed` / `emit_event_typed_body` /
  `emit_event_service_typed`. Serializes the parsed event/subject
  unions with camelCase lexicon fields (`modEventComment` /
  `modEventAcknowledge` / `modEventTakedown` / `repoRef` /
  `strongRef`, and the rest of the existing parse union). `Unknown`
  reuses `.original`. Raw Yojson `emit_event` / `emit_event_body` /
  `emit_event_service` stay unchanged. Does not fold leftover unused
  lexicon fields (`severityLevel` / `strikeCount` / `targetServices` /
  `isReporterMuted`). Live TestNetwork hop uses the typed path when
  Ozone is served; skip-if-not-served stays. No hosted chat / video /
  Tap / phone / contacts / push faked
- [#188](https://github.com/david-engelmann/atproto/pull/188): OAuth live
  Ozone hop (`test_live_oauth_ozone`) uses
  `Ozone.emit_event_service_typed` with typed `Comment` / `Repo_ref`
  variants (same pattern as
  [#187](https://github.com/david-engelmann/atproto/pull/187)
  `emit_event_typed` on `test_local_ozone.ml`). Skip-if-not-served /
  `classify_ozone` unchanged. Raw Yojson `emit_event_service` stays.
  Does not invent hosted chat / video / Tap / phone / contacts / push
- [#189](https://github.com/david-engelmann/atproto/pull/189): Xrpc
  helpers for Bluesky topics headers, same shape as
  `accept_labelers_header`. `topics_to_string` comma-joins topic
  strings (upstream array join). `topics_header` builds
  (`x-atproto-bsky-topics`, value); `legacy_topics_header` builds
  deprecated (`x-bsky-topics`, value); `topics_headers` returns the
  current pair and optionally the legacy pair for
  `Client.get_json ~extra`. `parse_topics` / `topics_from_headers`
  read comma-separated values and prefer the current header over
  legacy. Mirrors bluesky-social/atproto #5448 (`80d391a2`) client
  header names; does not bump the lexicon pin (`f0d4877a`). No
  hosted chat / video / Tap / phone / contacts / push faked
- [#190](https://github.com/david-engelmann/atproto/pull/190): typed
  Draft create/update helpers: `draft_to_json` /
  `create_draft_typed` / `update_draft_typed` /
  `create_draft_typed_body` / `update_draft_typed_body`. Serializes
  parsed `draft` via existing `draft_json` builders. Raw Yojson
  `create_draft` / `update_draft` stay unchanged. Unit tests for
  encode round-trip; live AppView hop uses the typed path when served.
  Does not invent leftover unused draft fields. No hosted chat / video /
  Tap / phone / contacts / push faked
- [#191](https://github.com/david-engelmann/atproto/pull/191): typed
  `Ozone.create_activity` encoding: `report_activity_to_json`
  plus `create_activity_typed` / `create_activity_typed_body`.
  Serializes the parsed `report_activity` union (`queueActivity` /
  `assignmentActivity` / `escalationActivity` / `closeActivity` /
  `reopenActivity` / `noteActivity`) with camelCase lexicon fields.
  `Unknown` reuses `.original`. Raw Yojson `create_activity` /
  `create_activity_body` stay unchanged. Does not invent leftover
  unused lexicon fields. Live TestNetwork leftover hop uses the typed
  body when Ozone is served; skip-if-not-served stays. No hosted chat /
  video / Tap / phone / contacts / push faked
- [#192](https://github.com/david-engelmann/atproto/pull/192): Repo
  create/put Yojson record helpers: `create_record_body` /
  `put_record_body` plus `create_record_json` / `put_record_json`.
  The `_json` paths take `record : Yojson.Safe.t` (same optional
  labels as the string versions) and return the existing
  `write_result` via `parse_write_result` (`uri` / `cid` / optional
  `commit`). String `create_record` / `put_record` stay unchanged and
  share the body builders. Does not invent leftover unused lexicon
  fields. No lexicon pin bump. No hosted chat / video / Tap / phone /
  contacts / push faked
- [#193](https://github.com/david-engelmann/atproto/pull/193): Repo
  `delete_record_body` plus `apply_writes_parsed`. The body builder
  uses the same lexicon field names as string `delete_record`
  (`repo` / `collection` / `rkey` / optional `swapRecord` /
  `swapCommit`) and shares `post_repo_write`. `apply_writes_parsed`
  calls `apply_writes` then `parse_apply_writes_result` (existing
  `apply_writes_result`: optional `commit` / `results`). String
  `delete_record` / `apply_writes` stay unchanged. Does not invent
  leftover unused lexicon fields. No lexicon pin bump. No hosted chat /
  video / Tap / phone / contacts / push faked
- [#194](https://github.com/david-engelmann/atproto/pull/194): Repo
  `blob_ref_to_json` for the upload→record loop. Encodes the standard
  AT blob object (`$type` / `ref.$link` / `mimeType` / `size`). Reuses
  `.original` when it is already a well-formed blob Assoc with those
  fields; otherwise builds from `cid` / `mime_type` / `size` via
  `Embed.blob_to_json`. `parse_blob_ref` / `upload_blob` stay
  unchanged. Does not invent leftover unused lexicon fields. No
  lexicon pin bump. No hosted chat / video / Tap / phone / contacts /
  push faked
- [#195](https://github.com/david-engelmann/atproto/pull/195):
  Moderation `create_report` Yojson body helpers:
  `create_report_body_from_strong_ref` /
  `create_report_body_from_repo_ref`. Uses existing `report_fields` /
  subject constructors (`reasonType` / `subject.$type` / optional
  `reason` / `modTool`). String `create_report_data_from_strong_ref` /
  `create_report_data_from_repo_ref` stay as `Yojson.Safe.to_string`
  wrappers. `create_report_with_*` posts the Yojson body via
  `Client.post_json` and still returns `report_response`. Does not
  invent leftover unused lexicon fields. No lexicon pin bump. No
  hosted chat / video / Tap / phone / contacts / push faked
- [#196](https://github.com/david-engelmann/atproto/pull/196): typed
  `Ozone.schedule_action` encoding: `schedule_action_typed` /
  `schedule_action_typed_body`. Same labeled args as raw
  `schedule_action`; optional `mod_tool` is the parsed record and
  encodes via `mod_tool_to_json`. `action` stays Yojson
  (`takedown_action`); `scheduling` still uses `scheduling_to_json`.
  Raw Yojson `schedule_action` / `schedule_action_body` stay unchanged.
  Does not invent leftover unused lexicon fields. No lexicon pin bump.
  No hosted chat / video / Tap / phone / contacts / push faked
- [#198](https://github.com/david-engelmann/atproto/pull/198): Graph
  mute/unmute Client + `unmute_actor_body`. JSON body helper matches
  `mute_actor_body` (`{"actor": ...}` only; no leftover unused
  unmuteActor fields). `mute_actor` / `unmute_actor` post via
  `Client.post_json` and still return `string` (empty procedure output
  stays `""`). Session `get_blocks` / `get_mutes` / `get_follows` /
  `get_followers` use `Client.get_json` with the `limit` query param
  and keep parsed return types. No lexicon pin bump. No hosted chat /
  video / Tap / phone / contacts / push faked
- [#197](https://github.com/david-engelmann/atproto/pull/197): Ozone
  `cancel_scheduled_actions` Yojson body helper
  (`cancel_scheduled_actions_body`). Lexicon fields only: `subjects`
  (array of strings) and optional `comment`. Existing
  `cancel_scheduled_actions` shares that body and still returns
  `batch_result`. Does not invent leftover unused lexicon fields. No
  lexicon pin bump. No hosted chat / video / Tap / phone / contacts /
  push faked
- [#199](https://github.com/david-engelmann/atproto/pull/199): Label
  `queryLabels` via `Client.get_json`: `query_labels` /
  `query_labels_parsed` share `query_labels_body` (`uriPatterns` /
  optional `sources` / `limit` / `cursor`) and call
  `Client.Client.get_json` instead of hand-rolled Cohttp headers.
  Parse return types and public signatures stay unchanged.
  `subscribeLabels` / WebSocket paths stay. Does not invent leftover
  unused lexicon fields. No lexicon pin bump. No hosted chat / video /
  Tap / phone / contacts / push faked
- [#200](https://github.com/david-engelmann/atproto/pull/200): Actor
  `getProfile` / `getProfiles` / `getSuggestions` / `searchActors` /
  `searchActorsTypeahead` via `Client.get_json`. Session helpers share
  `get_profile_body` / `get_profiles_body` / `get_suggestions_body` /
  `search_actors_body` (`actor` / repeated `actors` / `limit` / `q`)
  instead of hand-rolled Cohttp headers. Parse return types and
  public signatures stay unchanged. Preferences paths stay.
  Does not invent leftover unused lexicon fields. No lexicon pin
  bump. No hosted chat / video / Tap / phone / contacts / push faked
- [#201](https://github.com/david-engelmann/atproto/pull/201): Repo
  leftover JSON XRPC via Client. String `describe_repo` /
  `get_record` / `list_records` use `Client.get_text` and share
  `describe_repo_body` / `get_record_body` / `list_records_body` with
  the parsed helpers. `post_repo_write` (create/put/delete/applyWrites)
  uses `Client.post_json` and still returns `string` (empty procedure
  output stays `""`). `list_missing_blobs` uses `Client.get_json` and
  shares `list_missing_blobs_body`. Binary `upload_blob` / `import_repo`
  stay Cohttp. Does not invent leftover unused lexicon fields. No
  lexicon pin bump. No hosted chat / video / Tap / phone / contacts /
  push faked
- [#202](https://github.com/david-engelmann/atproto/pull/202):
  Feed leftover session JSON XRPC via Client: `get_author_feed` /
  `get_likes` / `get_post_thread` / `get_posts` / `get_reposted_by` /
  `get_timeline` / `get_feed_skeleton` share `get_author_feed_body` /
  `get_likes_body` / `get_post_thread_body` / `get_posts_body` /
  `get_reposted_by_body` / `get_timeline_body` /
  `get_feed_skeleton_body` (`actor` / `uri` / `cid` / repeated `uris` /
  `depth` / `algorithm` / `feed` / `limit` / optional `filter` /
  `includePins`) and call `Client.get_json` (raw `get_feed_skeleton`
  uses `Client.get_text`) instead of hand-rolled Cohttp headers.
  Parse return types and public signatures stay unchanged. Public
  `get_author_feed_page` / `get_feed_skeleton_parsed` stay. Does not
  invent leftover unused lexicon fields. No lexicon pin bump. No
  hosted chat / video / Tap / phone / contacts / push faked
- [#203](https://github.com/david-engelmann/atproto/pull/203):
  Notification leftover JSON XRPC via Client: `get_unread_count`
  uses `Client.get_json` (no leftover unused getUnreadCount query
  fields). `update_seen_body` is `{ "seenAt" }`; `update_seen` posts
  via `Client.post_json` and still returns `string` (empty procedure
  output stays `""`). Parse return types and public signatures stay
  unchanged. Push register/unregister stay listed not faked. Does not
  invent leftover unused lexicon fields. No lexicon pin bump. No
  hosted chat / video / Tap / phone / contacts / push faked
- [#204](https://github.com/david-engelmann/atproto/pull/204):
  Server leftover session JSON XRPC via Client. String
  `describe_server` / `list_app_passwords` / `get_account_invite_codes`
  use `Client.get_text` and share `get_account_invite_codes_body`
  (`includeUsed` / `createAvailable`) plus the empty query with
  `describe_server_parsed`. `create_account` uses `Client.post_json`
  and shares `create_account_body` with `create_account_at`.
  `create_invite_codes` shares `create_invite_codes_body`. Parse
  return types and public signatures stay unchanged. `get_service_auth`
  stays Cohttp. Does not invent leftover unused lexicon fields. No
  lexicon pin bump. No hosted chat / video / Tap / phone / contacts /
  push faked
- [#205](https://github.com/david-engelmann/atproto/pull/205):
  Session leftover JSON XRPC via Client: `get_session_request` uses
  `Client.get_text` (no leftover unused getSession query fields).
  `refresh_session` / `delete_session` post via `Client.post_json`
  with Bearer `refreshJwt` (no JSON body). `delete_session` still
  returns `string` (empty procedure output stays `""`). Parse return
  types and public signatures stay unchanged. `Session.session` is
  the same record as `Auth.session` so Client / App can hop without
  a Session cycle. `create_session` stays on Auth Cohttp. Does not
  invent leftover unused lexicon fields. No lexicon pin bump. No
  hosted chat / video / Tap / phone / contacts / push faked
- [#206](https://github.com/david-engelmann/atproto/pull/206):
  Identity resolveHandle/resolveDid via Client. `resolve_handle` /
  `resolve_did` / `resolve_identity` use `Client.get_json` and share
  `resolve_handle_body` / `resolve_did_body` / `resolve_identity_query`
  (`handle` / `did` / `identifier`). Host still defaults to `ATP_HOST`
  (not public AppView). `MethodNotImplemented` directory fallback
  stays. Parse return types and public signatures stay unchanged.
  Does not invent leftover unused lexicon fields. No lexicon pin
  bump. No hosted chat / video / Tap / phone / contacts / push faked
- [#207](https://github.com/david-engelmann/atproto/pull/207):
  Sync leftover JSON XRPC via Client. `get_latest_commit` /
  `get_repo_status` / `list_repos` / `list_blobs` / `list_hosts` /
  `get_host_status` / `list_repos_by_collection` use `Client.get_json`
  and share `get_latest_commit_body` / `get_repo_status_body` /
  `list_repos_body` / `list_blobs_body` / `list_hosts_body` /
  `get_host_status_body` / `list_repos_by_collection_body`. Host
  still defaults to `ATP_HOST` (not public AppView). `request_crawl`
  via `Client.post_json` still returns `string` (empty procedure
  output stays `""`). Binary `getRepo` / `getBlob` / `getBlocks` /
  `getRecord` CAR stay Cohttp. Parse return types and public
  signatures stay unchanged. Does not invent leftover unused lexicon
  fields. No lexicon pin bump. No hosted chat / video / Tap / phone /
  contacts / push faked
- [#208](https://github.com/david-engelmann/atproto/pull/208):
  Server getServiceAuth via Client: `get_service_auth` uses
  `Client.get_json` and shares `get_service_auth_body` (`aud` /
  optional `lxm` / `exp`) with `get_service_auth_url`. Reuses
  `Xrpc.service_auth_body` for aud / lxm validation. `service_auth`
  parse type and public signature stay unchanged. Auth
  `create_session` / `make_auth_token_request` stay Cohttp. Binary
  `upload_blob` / `import_repo` / Sync CAR stay Cohttp. Does not
  invent leftover unused lexicon fields. No lexicon pin bump. No
  hosted chat / video / Tap / phone / contacts / push faked
- [#209](https://github.com/david-engelmann/atproto/pull/209): Graph
  muteActorList / muteThread Yojson body helpers:
  `mute_actor_list_body` / `unmute_actor_list_body` /
  `mute_thread_body` / `unmute_thread_body`. JSON matches the pin
  `f0d4877a` lexicon (`{"list": ...}` / `{"root": ...}` only; no
  leftover unused muteActorList / unmuteActorList / muteThread /
  unmuteThread fields). `mute_actor_list` / `unmute_actor_list` /
  `mute_thread` / `unmute_thread` share those bodies via
  `Client.post_json`. Public signatures stay `unit`. No lexicon pin
  bump. No hosted chat / video / Tap / phone / contacts / push faked
- [#210](https://github.com/david-engelmann/atproto/pull/210):
  Notification putPreferences v1 + putActivitySubscription Yojson
  body helpers: `put_preferences_body` is `{ "priority" }`;
  `put_activity_subscription_body` is `{ "subject",
  "activitySubscription": { "post", "reply" } }`. Existing
  `put_preferences` / `put_activity_subscription` share those bodies
  via `Client.post_json`. Parse return types and public signatures
  stay unchanged. `putPreferencesV2` still uses `preferences_to_json`.
  Push register/unregister stay listed not faked. Does not invent
  leftover unused lexicon fields. No lexicon pin bump. No hosted
  chat / video / Tap / phone / contacts / push faked
- [#211](https://github.com/david-engelmann/atproto/pull/211): Ozone
  `list_scheduled_actions` Yojson body helper
  (`list_scheduled_actions_body`). Lexicon fields only: required
  `statuses` (array of strings) and optional `startsAfter` /
  `endsBefore` / `subjects` / `limit` / `cursor`. Existing
  `list_scheduled_actions` shares that body and still returns
  `scheduled_actions`. Does not invent leftover unused lexicon
  fields. No lexicon pin bump. No hosted chat / video / Tap / phone /
  contacts / push faked
- [#212](https://github.com/david-engelmann/atproto/pull/212):
  Ozone communication `updateTemplate` Yojson body helper
  (`update_template_body`). Extracts current params only: required
  `id`, optional `name` / `contentMarkdown` / `subject` /
  `updatedBy` / `disabled`. Does not invent `lang`.
  `update_template` shares that body and still returns `template`.
  Also `delete_template_body` (`{ "id" }`) shared with
  `delete_template`. Sibling of `create_template_body`. No lexicon
  pin bump. No hosted chat / video / Tap / phone / contacts / push
  faked
- [#213](https://github.com/david-engelmann/atproto/pull/213): Ozone
  `query_safelink_rules` Yojson body helper
  (`query_safelink_rules_body`). Lexicon fields only: optional
  `cursor` / `limit` / `urls` / `patternType` / `actions` / `reason`
  / `createdBy` / `sortDirection` as currently sent (pin
  `f0d4877a03`). Existing `query_safelink_rules` shares that body and
  still returns `url_rules`. Does not invent leftover unused lexicon
  fields. No leftover live hop (live queryRules already exists). No
  lexicon pin bump. No hosted chat / video / Tap / phone / contacts /
  push faked
- [#214](https://github.com/david-engelmann/atproto/pull/214): Ozone
  report leftover Yojson POST body helpers
  (`assign_report_moderator_body` / `reassign_queue_body` /
  `refresh_stats_body` / `close_reports_body`). Lexicon fields only:
  assignModerator `reportId` plus optional `queueId` / `did` /
  `isPermanent`; reassignQueue `reportId` / `queueId` plus optional
  `comment`; refreshStats `startDate` / `endDate` plus optional
  `queueIds`; closeReports `subject` plus optional `reportTypes` /
  `internalNote` / `isAutomated`. Existing `assign_report_moderator` /
  `reassign_queue` / `refresh_stats` / `close_reports` share those
  bodies. Public signatures unchanged. Does not invent leftover unused
  lexicon fields. No lexicon pin bump. No hosted chat / video / Tap /
  phone / contacts / push faked
- [#218](https://github.com/david-engelmann/atproto/pull/218): Site typed record encodes: `document_to_json` /
  `publication_to_json` / `recommend_to_json` /
  `subscription_to_json`. Serializes parsed `document` /
  `publication` / `recommend` / `subscription` via existing
  builders (siblings of `theme_to_json` / `contributor_to_json` /
  `parse_*`). Lexicon fields only (`site.standard.document` /
  `publication` / `graph.recommend` / `graph.subscription`); does
  not invent leftover unused Site fields. Existing Yojson builders
  and parse types stay unchanged. Unit tests for encode
  round-trip; no live `Repo.create_record` hop. No lexicon pin
  bump. No hosted chat / video / Tap / phone / contacts / push
  faked
- [#215](https://github.com/david-engelmann/atproto/pull/215): Ozone
  set/setting/team leftover Yojson POST body helpers
  (`upsert_set_body` / `add_set_values_body` / `delete_set_values_body` /
  `upsert_option_body` / `update_member_body`). Lexicon fields only:
  upsertSet `name` plus optional `description`; addValues /
  deleteValues `name` / `values`; upsertOption `key` / `scope` /
  `value` plus optional `description` (does not invent leftover unused
  `managerRole`); updateMember `did` plus optional `role` / `disabled`.
  Existing `upsert_set` / `add_set_values` / `delete_set_values` /
  `upsert_option` / `update_member` share those bodies. Public
  signatures unchanged. Skips empty/`{}` leftovers (`delete_set` /
  `add_member` / `delete_member` / `remove_options` stay inline).
  Does not invent leftover unused lexicon fields. No lexicon pin
  bump. No hosted chat / video / Tap / phone / contacts / push faked
- [#216](https://github.com/david-engelmann/atproto/pull/216):
  Ozone queue leftover Yojson POST body helpers
  (`assign_queue_moderator_body` / `unassign_queue_moderator_body` /
  `route_reports_body`). Extracts current params only: required
  `queueId` / `did` for queue assign/unassign, required
  `startReportId` / `endReportId` for `routeReports`. Does not
  invent leftover unused queue fields. Existing
  `assign_queue_moderator` / `unassign_queue_moderator` /
  `route_reports` share those bodies. Public signatures unchanged.
  No lexicon pin bump. No hosted chat / video / Tap / phone /
  contacts / push faked
- [#219](https://github.com/david-engelmann/atproto/pull/219): Ozone
  `query_events` / `query_statuses` query-pair helpers
  (`query_events_body` / `query_statuses_body`). Currently sent
  fields only: `types` / `createdBy` / `subject` / `limit` / `cursor`
  for events; `subject` / `comment` / `reviewState` / `limit` /
  `cursor` for statuses (pin `f0d4877a03`). Existing `query_events` /
  `query_events_service` / `query_statuses` share those pairs.
  Distinct from #213 POST `query_safelink_rules_body`. Does not
  invent leftover unused queryEvents / queryStatuses fields. No
  leftover live hop (live queryEvents / queryStatuses already exist).
  No lexicon pin bump. No hosted chat / video / Tap / phone /
  contacts / push faked
- [#220](https://github.com/david-engelmann/atproto/pull/220): Feed
  leftover AppView query-pair helpers (`get_feed_body` /
  `get_list_feed_body` / `get_actor_feeds_body` /
  `search_posts_body`). Currently sent fields only: `feed` / `limit` /
  `cursor` for getFeed; `list` / `limit` / `cursor` for getListFeed;
  `actor` / `limit` / `cursor` for getActorFeeds; `q` / `sort` /
  `since` / `until` / `mentions` / `author` / `lang` / `domain` /
  `url` / `limit` / `cursor` for searchPosts (pin `f0d4877a03`).
  Existing `get_feed` / `get_list_feed` / `get_actor_feeds` /
  `search_posts` share those pairs. Distinct from #202 session
  `get_author_feed_body` / `get_likes_body` /
  `get_post_thread_body` / `get_posts_body` / `get_reposted_by_body` /
  `get_timeline_body` / `get_feed_skeleton_body`. Does not invent
  leftover unused getFeed / getListFeed / getActorFeeds / searchPosts
  fields. No leftover live hop (live getFeed / getListFeed /
  getActorFeeds / searchPosts already exist). No lexicon pin bump. No
  hosted chat / video / Tap / phone / contacts / push faked
- [#221](https://github.com/david-engelmann/atproto/pull/221): Graph
  leftover AppView query-pair helpers (`get_list_body` /
  `get_lists_body` / `get_actor_starter_packs_body` /
  `search_starter_packs_body` / `get_relationships_body` /
  `get_known_followers_body`). Currently sent fields only: `list` /
  `limit` / `cursor` for getList; `actor` / `limit` / `cursor` for
  getLists / getActorStarterPacks / getKnownFollowers; `q` / `limit` /
  `cursor` for searchStarterPacks / searchStarterPacksV2; `actor` /
  repeated `others` for getRelationships (pin `f0d4877a03`). Existing
  `get_list` / `get_lists` / `get_actor_starter_packs` /
  `search_starter_packs` / `search_starter_packs_v2` /
  `get_relationships` / `get_known_followers` share those pairs.
  Distinct from #198 session leftovers and existing
  `follow_page_pairs`. Skips tiny one-liners (`get_starter_pack`
  `[("starterPack", ...)]`, `get_starter_packs` `repeat_param`,
  `get_suggested_follows_by_actor` `[("actor", ...)]`). Does not
  invent leftover unused getList / getLists / getActorStarterPacks /
  searchStarterPacks / getRelationships / getKnownFollowers fields.
  No leftover live hop (live getList / getLists /
  getActorStarterPacks / searchStarterPacks / getRelationships /
  getKnownFollowers already exist). No lexicon pin bump. No hosted
  chat / video / Tap / phone / contacts / push faked
- [#222](https://github.com/david-engelmann/atproto/pull/222): Feed
  leftover searchPostsV2 / getQuotes / getActorLikes query-pair helpers
  (`search_posts_v2_body` / `get_quotes_body` / `get_actor_likes_body`).
  Currently sent fields only: optional `query` / `sort` / repeated
  `authors` / `mentions` / `domains` / `urls` / `embeddedAtUris` /
  `hashtags` / `excludeAuthors` / `excludeMentions` / `excludeDomains` /
  `excludeUrls` / `excludeEmbeddedAtUris` / `excludeHashtags` / `since`
  / `until` / `allTime` / repeated `languages` / `excludeLanguages` /
  `hasMedia` / `hasVideo` / `replyParentUri` / `threadRootUri` /
  `excludeReplies` / `repliesOnly` / `following` / `queryLanguage` /
  `limit` / `cursor` for searchPostsV2; `uri` / `cid` / `limit` /
  `cursor` for getQuotes; `actor` / `limit` / `cursor` for
  getActorLikes (pin `f0d4877a03`). Existing `search_posts_v2` /
  `get_quotes` / `get_actor_likes` share those pairs. Siblings of #220
  `get_feed_body` / `get_list_feed_body` / `get_actor_feeds_body` /
  `search_posts_body`. On top of #221 → #220. Does not invent leftover
  unused searchPostsV2 / getQuotes / getActorLikes fields. No leftover
  live hop (live searchPostsV2 / getQuotes / getActorLikes already
  exist). No lexicon pin bump. No hosted chat / video / Tap / phone /
  contacts / push faked
- [#223](https://github.com/david-engelmann/atproto/pull/223): Unspecced
  leftover searchPostsSkeleton / searchActorsSkeleton /
  searchStarterPacksSkeleton query-pair helpers
  (`search_posts_skeleton_body` / `search_actors_skeleton_body` /
  `search_starter_packs_skeleton_body`). Currently sent fields only:
  required `q`, optional `sort` / `since` / `until` / `mentions` /
  `author` / `lang` / `domain` / `url` / `viewer` / `limit` / `cursor`
  for searchPostsSkeleton; `q` / `viewer` / `typeahead` / `limit` /
  `cursor` for searchActorsSkeleton; `q` / `viewer` / `limit` /
  `cursor` for searchStarterPacksSkeleton (pin `f0d4877a03`). Existing
  `search_posts_skeleton` / `search_actors_skeleton` /
  `search_starter_packs_skeleton` share those pairs. Distinct from
  #220 / #222 Feed searchPosts helpers. On top of #222. Does not
  invent leftover unused searchPostsSkeleton / searchActorsSkeleton /
  searchStarterPacksSkeleton fields. No leftover live hop (live
  searchPostsSkeleton already exists). No lexicon pin bump. No hosted
  chat / video / Tap / phone / contacts / push faked
- [#224](https://github.com/david-engelmann/atproto/pull/224): Unspecced
  leftover getSuggestionsSkeleton / getPostThreadV2 / suggested-users
  query-pair helpers (`get_suggestions_skeleton_body` /
  `get_post_thread_v2_body` / `get_suggested_users_body` /
  `get_suggested_users_skeleton_body`). Currently sent fields only:
  optional `viewer` / `limit` / `cursor` / `relativeToDid` for
  getSuggestionsSkeleton; required `anchor`, optional `above` /
  `below` / `branchingFactor` / `sort` for getPostThreadV2; optional
  `category` / `limit` for getSuggestedUsers and the onboarding /
  explore / seeMore siblings; optional `viewer` / `category` / `limit`
  for getSuggestedUsersSkeleton and those skeleton siblings (pin
  `f0d4877a03`). Existing `get_suggestions_skeleton` /
  `get_post_thread_v2` / `get_suggested_users` /
  `get_suggested_onboarding_users` / `get_suggested_users_for_explore`
  / `get_suggested_users_for_see_more` / `get_suggested_users_skeleton`
  / `get_onboarding_suggested_users_skeleton` /
  `get_suggested_users_for_explore_skeleton` /
  `get_suggested_users_for_see_more_skeleton` share those pairs.
  Distinct from #223 search skeleton helpers. On top of #223. Does not
  invent leftover unused getSuggestionsSkeleton / getPostThreadV2 /
  getSuggestedUsers fields. Skips thin viewer+limit / limit-only
  discover / feed / starter-pack one-liners and
  getPostThreadOtherV2 `[("anchor", ...)]`. No leftover live hop
  (live getPostThreadV2 already exists). No lexicon pin bump. No
  hosted chat / video / Tap / phone / contacts / push faked
- [#225](https://github.com/david-engelmann/atproto/pull/225): Ozone
  leftover query_reports / search_repos query-pair helpers
  (`query_reports_body` / `search_repos_body`). Currently sent
  fields only: required `status`, optional `queueId` / `reportTypes`
  / `subject` / `did` / `subjectType` / `collections` /
  `reportedAfter` / `reportedBefore` / `isMuted` / `assignedTo` /
  `sortField` / `sortDirection` / `limit` / `cursor` for
  queryReports; optional `q` / `term` / `limit` / `cursor` for
  searchRepos (pin `f0d4877a03`). Existing `query_reports` /
  `search_repos` share those pairs. Distinct from #219
  `query_events_body` / `query_statuses_body` and #214 report POST
  leftovers. On top of #224. Does not invent leftover unused
  queryReports / searchRepos fields. Skips thin Ozone GET
  one-liners (`get_event` / `get_repo` / `get_report` /
  `get_latest_report`) and leftover Admin GET pairs. No leftover
  live hop (live queryReports / searchRepos already exist). No
  lexicon pin bump. No hosted chat / video / Tap / phone /
  contacts / push faked
- [#226](https://github.com/david-engelmann/atproto/pull/226):
  Notification leftover list_notifications query-pair helpers
  (`list_notifications_body`). Currently sent fields only: optional
  `reasons` / `priority` / `cursor` / `seenAt` / `limit` (pin
  `f0d4877a03`). Existing `list_notifications` /
  `list_notifications_page` share those pairs. Distinct from #210
  putPreferences / putActivitySubscription POST leftovers and #203
  getUnreadCount / updateSeen. On top of #225. Does not invent
  leftover unused listNotifications fields. Skips thin
  `list_activity_subscriptions` limit/cursor. No leftover live hop
  (live listNotifications already exists). No lexicon pin bump. No
  hosted chat / video / Tap / phone / contacts / push faked
- [#227](https://github.com/david-engelmann/atproto/pull/227): Feed
  leftover getAuthorFeed / getFeedSkeleton query-pair helpers
  (`get_author_feed_body` / `get_feed_skeleton_body`). Currently sent
  fields only: required `actor`, optional `limit` / `cursor` /
  `filter` / `includePins` for getAuthorFeed; required `feed`,
  optional `limit` / `cursor` for getFeedSkeleton (pin `f0d4877a03`).
  Existing `get_author_feed` / `get_author_feed_page` /
  `get_feed_skeleton` / `get_feed_skeleton_parsed` share those pairs.
  Distinct from #202 session leftovers and #220 / #222 AppView
  leftovers. On top of #226. Does not invent leftover unused
  getAuthorFeed / getFeedSkeleton fields. No leftover live hop
  (live getAuthorFeed / getFeedSkeleton already exist). No lexicon
  pin bump. No hosted chat / video / Tap / phone / contacts / push
  faked
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
- [#133](https://github.com/david-engelmann/atproto/pull/133):
  function-level odoc on remaining Tid / At_uri / Dag_cbor /
  K256 / Did_key / Did_web helpers (`of_int64`, `to_string`, `as_text`,
  `low_s`, `is_did_key`, `is_web_did`). Comment-only
- [#138](https://github.com/david-engelmann/atproto/pull/138):
  function-level odoc on remaining Session / Moderation / Temp helpers
  (`refresh_token_from_session`, `get_session_request`, reason-type
  constants, `create_report_data_from_*`, `check_signup_queue`,
  `dereference_scope`, `add_reserved_handle`,
  `request_phone_verification`, `revoke_account_credentials`).
  Comment-only; hosted-only phone verification stays listed not faked
- [#141](https://github.com/david-engelmann/atproto/pull/141): remaining
  function-level odoc on Repo XRPC wrappers
  (`describe_repo` / `describe_repo_parsed`, `list_records` /
  `list_records_parsed`, `get_record_parsed`, `put_record`,
  `delete_record`, `apply_writes_body`, `write_op_to_json`,
  `upload_blob`, `verify_blob_bytes`, `list_missing_blobs`,
  `import_repo`). Comment-only
- [#142](https://github.com/david-engelmann/atproto/pull/142): remaining
  function-level odoc on Sync XRPC wrappers
  (`get_repo_car`, `get_blob`, `download_image`, `get_blocks` /
  `get_blocks_bytes` / `get_blocks_car`, `get_record` /
  `get_record_car`, `list_blobs`, `list_repos`, `get_repo_status`,
  `list_hosts`, `get_host_status`, `list_repos_by_collection`,
  `request_crawl` / `request_crawl_body`). Comment-only. Deprecated
  `get_head` / `get_checkout` / `notify_of_update` wrap
  `getLatestCommit` / `getRepo` / `requestCrawl` (coverage-skips, not
  leftover hops)
- [#140](https://github.com/david-engelmann/atproto/pull/140): remaining
  function-level odoc on Germnetwork / Lexicon (`nsid_declaration`,
  `show_*`, `bytes_to_json` / `bytes_of_json`, `message_me` /
  `parse_declaration`, `of_json`, lookup / `to_ocaml`). Comment-only
- [#143](https://github.com/david-engelmann/atproto/pull/143): remaining
  function-level odoc on Oauth_scope / Syntax
  (`resource_name`, `resource_of_string`, `collections_of`,
  `actions_of`, `lxm_of`, `aud_of`, `validate`, `to_string`,
  `is_valid_datetime` / `is_valid_language`, `parse_nsid` /
  `parse_did_ref`, `normalize_handle`). Comment-only
- [#144](https://github.com/david-engelmann/atproto/pull/144): remaining
  function-level odoc on Feed XRPC wrappers
  (`get_reposted_by`, `get_feed_skeleton` / `get_feed_skeleton_parsed`,
  `get_feed_generator` / `get_feed_generators`, `get_actor_feeds`,
  `get_suggested_feeds`, `describe_feed_generator`, `search_posts_v2`,
  `get_quotes`, `get_actor_likes`, `send_interactions` /
  `send_interactions_body`). Comment-only; does not host a feed
  generator. Hosted-only video / chat / Tap stay listed not faked
- [#145](https://github.com/david-engelmann/atproto/pull/145): remaining
  function-level odoc on Graph XRPC wrappers
  (`get_followers_page` / `get_follows_page`, `mute_actor_body`,
  `get_list_mutes` / `get_list_blocks`, `mute_actor_list` /
  `unmute_actor_list`, `mute_thread` / `unmute_thread`,
  `get_starter_pack` / `get_starter_packs` / `get_actor_starter_packs`,
  `search_starter_packs` / `search_starter_packs_v2`,
  `get_lists_with_membership` / `get_starter_packs_with_membership`,
  `get_known_followers`, `get_suggested_follows_by_actor`). Comment-only
- [#146](https://github.com/david-engelmann/atproto/pull/146): remaining
  function-level odoc on Notification XRPC wrappers
  (`put_preferences` / `put_preferences_v2`,
  `list_activity_subscriptions`, `put_activity_subscription`,
  `register_push` / `unregister_push`, `list_notifications_page`).
  Comment-only; client wrappers for a hosted push service stay listed
  not faked. Hosted-only video / chat / Tap stay listed not faked
- [#147](https://github.com/david-engelmann/atproto/pull/147): remaining
  function-level odoc on Records builders
  (`repost`, `block`, `listblock`, `list` / `listitem`,
  `referencelistoptout`, `starterpack`, `profile`, `status`,
  `content_visibility_declaration`, `verification`, `threadgate` /
  `postgate`, `generator`, `labeler_service`,
  `notification_declaration`, `lexicon_schema`, `chat_declaration`).
  Comment-only. Hosted-only chat / video / Tap stay listed not faked
- [#148](https://github.com/david-engelmann/atproto/pull/148): remaining
  function-level odoc on Unspecced XRPC wrappers
  (`search_actors_skeleton` / `search_starter_packs_skeleton`,
  `get_config`, `get_popular_feed_generators`, `get_tagged_suggestions`,
  `get_age_assurance_state`, `init_age_assurance` /
  `init_age_assurance_body`, `get_trends` / `get_trends_skeleton`,
  `get_suggestions_skeleton`, `get_suggested_*` /
  `get_onboarding_*` skeletons, `get_post_thread_v2` /
  `get_post_thread_other_v2`). Comment-only. Hosted-only video / chat /
  Tap stay listed not faked
- [#149](https://github.com/david-engelmann/atproto/pull/149): remaining
  function-level odoc on Label helpers
  (`query_labels_parsed`, `query_labels_body`, `subscribe` /
  `subscribe_one`, `encode_unsigned` / `encode_signed`). Comment-only.
  Hosted-only chat / video / Tap stay listed not faked
- [#151](https://github.com/david-engelmann/atproto/pull/151): remaining
  function-level odoc on Chat XRPC wrappers
  (`get_convo_for_members`, `update_read`, `mute_convo` / `unmute_convo`,
  `accept_convo` / `leave_convo`, `add_reaction` / `remove_reaction`,
  `delete_message_for_self`, `get_convo_availability`, `get_log`,
  `get_unread_counts`, `list_convo_requests`, `send_message_batch`,
  `update_all_read`, `lock_convo` / `unlock_convo`, `add_members` /
  `remove_members`, `edit_group`, `get_convo_members`, `create_group`,
  join-link / join-request helpers, `get_notification_preferences` /
  `put_notification_preferences`, `get_actor_status`, `delete_account`,
  `export_account_data`, moderation views / `subscribe_mod_events`).
  Comment-only. Hosted-only chat / video / Tap stay listed not faked
- [#156](https://github.com/david-engelmann/atproto/pull/156): remaining
  function-level odoc on Contact XRPC wrappers
  (`get_sync_status`, `dismiss_match`, `remove_data`,
  `start_phone_verification`, `verify_phone`, `send_notification`,
  and matching `*_body` helpers). Comment-only; client only.
  Phone / contacts stay listed not faked
- [#157](https://github.com/david-engelmann/atproto/pull/157): remaining
  function-level odoc on public Video helpers. Comment-only;
  Video stays client-only (no hosted transcoder)
- [#158](https://github.com/david-engelmann/atproto/pull/158): remaining
  function-level odoc on Site record builders (public Site NSIDs,
  nested builders, and `*_to_json` helpers). Comment-only
- [#159](https://github.com/david-engelmann/atproto/pull/159): remaining
  function-level odoc on public Draft helpers
  (`update_draft` / `delete_draft` plus `draft_json`,
  `draft_post_json`, embed-json, and `*_draft_body` builders).
  Comment-only
- [#160](https://github.com/david-engelmann/atproto/pull/160): remaining
  function-level odoc on Ozone constructors. Comment-only;
  skips `parse_*` internals and already-documented XRPC wrappers
- [#161](https://github.com/david-engelmann/atproto/pull/161): remaining
  function-level odoc on public Ageassurance helpers
  (`begin_body`, `get_config`). Comment-only
- [#165](https://github.com/david-engelmann/atproto/pull/165): odoc landing
  map in `doc/index.mld` adds Draft / Contact / Ageassurance / Site
  to AppView `{!modules:}` and Germnetwork beside the other public
  modules
- [#166](https://github.com/david-engelmann/atproto/pull/166): remaining
  function-level odoc on public `Embed.get_embed_external_view`
  (`app.bsky.embed.getEmbedExternalView`). Comment-only; skips
  `parse_embed_external_view`
- [#170](https://github.com/david-engelmann/atproto/pull/170): odoc landing
  leftover public protocol modules (Admin / Temp / Moderation, Embed /
  Facet on AppView, Tid / At_uri / Syntax). Docs-only; skips codec /
  HTTP internals
- [#171](https://github.com/david-engelmann/atproto/pull/171): compiled-only
  `examples/offline.ml` typechecks for leftover Ageassurance /
  Unspecced constructors and parsers (`begin_body`, `parse_config`,
  `init_age_assurance_body`, `parse_age_assurance_state`). No live hops
- [#172](https://github.com/david-engelmann/atproto/pull/172): CI installs
  the local generated opam package after the source-tree build, then
  builds and runs an isolated downstream Dune consumer under
  `RUNNER_TEMP`. Not an opam-repository publish
- [#173](https://github.com/david-engelmann/atproto/pull/173): Point
  CHANGELOG and remaining-gaps through #172. Docs-only notes hygiene
- [#174](https://github.com/david-engelmann/atproto/pull/174): Document
  libzstd system dependency for installs (README +
  `.github/CONTRIBUTING.md`). Jane Street `zstandard` / Jetstream
  dict-zstd; Ubuntu/Debian `libzstd-dev`, Homebrew `zstd`
- [#175](https://github.com/david-engelmann/atproto/pull/175): Point
  CHANGELOG and remaining-gaps through #174. Docs-only notes hygiene
- [#176](https://github.com/david-engelmann/atproto/pull/176): Add Error
  to the odoc landing map and libzstd install notes (`doc/index.mld`
  `{1 Install}` plus `.github/ISSUE_TEMPLATE/03-install.md`). Jane Street
  `zstandard` / Jetstream dict-zstd; Ubuntu/Debian `libzstd-dev`,
  Homebrew `zstd`
- [#177](https://github.com/david-engelmann/atproto/pull/177): remaining
  function-level odoc on public `Client.get_json_h2` /
  `Client.post_json_h2` (`Http_client` HTTP/2 TLS; hosts with an
  explicit port stay on Cohttp) plus one-line odoc on
  `Feed.filter_posts_*` `getAuthorFeed` knownValues. Comment-only
- [#178](https://github.com/david-engelmann/atproto/pull/178): Point
  CHANGELOG and remaining-gaps through #177 (`586b4877`). Docs-only
  notes hygiene
- [#179](https://github.com/david-engelmann/atproto/pull/179): Add
  merge-when-green squash automation (`95b6eb57`; workflow + script +
  CONTRIBUTING). Docs-only allowlist excludes `.github/scripts` and
  workflows. Green non-draft same-repo/Dependabot PRs to `main` may
  auto-squash-merge (see CONTRIBUTING)
- [#180](https://github.com/david-engelmann/atproto/pull/180): Point
  CHANGELOG and remaining-gaps through #179 (`82d8b543`). Docs-only
  notes hygiene
- [#181](https://github.com/david-engelmann/atproto/pull/181): official
  lexicon pin bluesky-social/atproto `f0d4877a` (the commit that added
  `updatedAt` on `app.bsky.actor.defs#interestsPref`, upstream
  [#5481](https://github.com/bluesky-social/atproto/pull/5481)).
  `Actor.interests_pref` = `{ tags; updated_at : string option }`. No
  new NSIDs; hosted-only chat / video / Tap stay listed not faked
- [#182](https://github.com/david-engelmann/atproto/pull/182): Ignore
  lexicon-pin drift in merge-when-green (`84040dc0`). Pin drift is
  advisory; a failing lexicon-pin check must not deadlock docs-only
  or full-mode merges
- [#183](https://github.com/david-engelmann/atproto/pull/183): Point
  CHANGELOG and remaining-gaps through #182 (`7fd95c86`). Docs-only
  notes hygiene
- [#184](https://github.com/david-engelmann/atproto/pull/184): live leftover
  TestNetwork hops for AppView and PDS
  `app.bsky.actor.getPreferences` / `putPreferences` that assert
  `interestsPref.updatedAt` when the served JSON includes it. Skip if
  not served or TestNetwork policy. Parse + `examples/offline.ml`
  coverage shipped in [#181](https://github.com/david-engelmann/atproto/pull/181).
  Hosted-only chat / video / Tap / phone / contacts / push stay listed
  not faked
- [#185](https://github.com/david-engelmann/atproto/pull/185): typed
  Actor `putPreferences` encoding
  (`preference_kind_to_json` / `preference_to_json` /
  `preferences_to_json` / `put_preferences_typed` /
  `put_preferences_typed_body`). `interestsPref` writes optional
  `updatedAt` from `interests_pref.updated_at`. `Other` reuses
  `.original`. Existing raw Yojson `put_preferences` call sites stay
  valid. Unit tests for encode with/without `updated_at` and
  parse→encode→parse. Live leftover hops stay in
  [#184](https://github.com/david-engelmann/atproto/pull/184). No
  hosted services faked
- [#186](https://github.com/david-engelmann/atproto/pull/186): typed
  `Ozone.get_account_preferences` via `Actor.parse_preference`.
  `account_preferences` is `{ preferences : Actor.preference list }`.
  Unit + live leftover hop assert typed kinds (and
  `interestsPref.updatedAt` when present). Compile-breaking; no
  `emit_event_typed`. Hosted-only chat / video / Tap / phone /
  contacts / push stay listed not faked
- [#187](https://github.com/david-engelmann/atproto/pull/187): typed
  `Ozone.emit_event` encoding (`event_to_json` /
  `subject_to_json` / `mod_tool_to_json` / `emit_event_typed` /
  `emit_event_typed_body` / `emit_event_service_typed`). Encodes the
  parsed event/subject unions; `Unknown` reuses `.original`. Existing
  raw Yojson `emit_event` call sites stay valid. Unit tests for
  comment / acknowledge / takedown and repo_ref / strong_ref plus
  parse→encode→parse. Live leftover hop uses the typed Session path
  when served. Does not invent leftover unused emitEvent fields.
  Hosted-only chat / video / Tap / phone / contacts / push stay
  listed not faked
- [#188](https://github.com/david-engelmann/atproto/pull/188): OAuth live
  Ozone hop uses `emit_event_service_typed` (`Comment` / `Repo_ref`).
  Skip-if-not-served / `classify_ozone` unchanged. Hosted-only chat /
  video / Tap / phone / contacts / push stay listed not faked
- [#191](https://github.com/david-engelmann/atproto/pull/191): typed
  `Ozone.create_activity` encoding (`report_activity_to_json` /
  `create_activity_typed` / `create_activity_typed_body`). Encodes the
  parsed report-activity union; `Unknown` reuses `.original`. Existing
  raw Yojson `create_activity` call sites stay valid. Unit tests for
  note / close / queue / assignment / escalation / reopen plus
  parse→encode→parse. Live leftover hop uses the typed body when
  served. Does not invent leftover unused createActivity fields.
  Hosted-only chat / video / Tap / phone / contacts / push stay
  listed not faked
- [#192](https://github.com/david-engelmann/atproto/pull/192): Repo
  create/put Yojson record helpers (`create_record_body` /
  `put_record_body` / `create_record_json` / `put_record_json`).
  `_json` paths take `record : Yojson.Safe.t` and return `write_result`
  via `parse_write_result`. String `create_record` / `put_record`
  unchanged and share the body builders. Does not invent leftover
  unused createRecord / putRecord fields. Hosted-only chat / video /
  Tap / phone / contacts / push stay listed not faked
- [#193](https://github.com/david-engelmann/atproto/pull/193): Repo
  `delete_record_body` plus `apply_writes_parsed`. String
  `delete_record` shares the body builder via `post_repo_write`.
  `apply_writes_parsed` calls `apply_writes` then
  `parse_apply_writes_result`. Raw-string `apply_writes` unchanged.
  Does not invent leftover unused deleteRecord / applyWrites fields.
  Hosted-only chat / video / Tap / phone / contacts / push stay
  listed not faked
- [#194](https://github.com/david-engelmann/atproto/pull/194): Repo
  `blob_ref_to_json` for upload→record (`$type` / `ref.$link` /
  `mimeType` / `size`). Reuses `.original` when it is already a
  well-formed blob Assoc; otherwise builds from `cid` / `mime_type` /
  `size`. `parse_blob_ref` / `upload_blob` unchanged. Does not invent
  leftover unused blob fields. Hosted-only chat / video / Tap / phone
  / contacts / push stay listed not faked
- [#195](https://github.com/david-engelmann/atproto/pull/195):
  Moderation `create_report` Yojson body helpers
  (`create_report_body_from_strong_ref` /
  `create_report_body_from_repo_ref`). String
  `create_report_data_from_*` wrappers unchanged. `create_report_with_*`
  posts via `Client.post_json` and still returns `report_response`.
  Does not invent leftover unused createReport fields. Hosted-only
  chat / video / Tap / phone / contacts / push stay listed not faked
- [#196](https://github.com/david-engelmann/atproto/pull/196): typed
  `Ozone.schedule_action` encoding (`schedule_action_typed` /
  `schedule_action_typed_body`). Optional `mod_tool` is the parsed
  record (`mod_tool_to_json`). `action` stays Yojson; `scheduling`
  still uses `scheduling_to_json`. Raw Yojson `schedule_action` /
  `schedule_action_body` unchanged. Does not invent leftover unused
  scheduleAction fields. Hosted-only chat / video / Tap / phone /
  contacts / push stay listed not faked
- [#198](https://github.com/david-engelmann/atproto/pull/198): Graph
  mute/unmute Client + `unmute_actor_body`. `mute_actor` /
  `unmute_actor` via `Client.post_json` keep `string`. Session
  `get_blocks` / `get_mutes` / `get_follows` / `get_followers` via
  `Client.get_json` with `limit`. Does not invent leftover unused
  unmuteActor fields. Hosted-only chat / video / Tap / phone /
  contacts / push stay listed not faked
- [#197](https://github.com/david-engelmann/atproto/pull/197): Ozone
  `cancel_scheduled_actions` Yojson body helper
  (`cancel_scheduled_actions_body`). Lexicon fields only: `subjects`
  (array of strings) and optional `comment`. `cancel_scheduled_actions`
  shares that body and still returns `batch_result`. Does not invent
  leftover unused cancelScheduledActions fields. Hosted-only chat /
  video / Tap / phone / contacts / push stay listed not faked
- [#199](https://github.com/david-engelmann/atproto/pull/199): Label
  `queryLabels` via `Client.get_json`. `query_labels` /
  `query_labels_parsed` share `query_labels_body` (`uriPatterns` /
  optional `sources` / `limit` / `cursor`). `subscribeLabels` /
  WebSocket paths stay. Does not invent leftover unused queryLabels
  fields. Hosted-only chat / video / Tap / phone / contacts / push
  stay listed not faked
- [#200](https://github.com/david-engelmann/atproto/pull/200): Actor
  profile/search/suggestions via `Client.get_json`. `get_profile` /
  `get_profiles` / `get_suggestions` / `search_actors` /
  `search_actors_typeahead` share query-pair helpers
  (`get_profile_body` / `get_profiles_body` / `get_suggestions_body` /
  `search_actors_body`). Parsed return types stay. Does not invent
  leftover unused actor query fields. Hosted-only chat / video / Tap /
  phone / contacts / push stay listed not faked
- [#201](https://github.com/david-engelmann/atproto/pull/201): Repo
  leftover JSON XRPC via Client. `describe_repo` / `get_record` /
  `list_records` share query-pair helpers with parsed paths;
  `post_repo_write` via `Client.post_json` keeps `string`;
  `list_missing_blobs` shares `list_missing_blobs_body`. Binary
  `upload_blob` / `import_repo` stay Cohttp. Does not invent leftover
  unused repo query fields. Hosted-only chat / video / Tap / phone /
  contacts / push stay listed not faked
- [#204](https://github.com/david-engelmann/atproto/pull/204): Server
  leftover session JSON XRPC via Client. `describe_server` /
  `list_app_passwords` / `get_account_invite_codes` via
  `Client.get_text`; `create_account` via `Client.post_json` shares
  `create_account_body`; `create_invite_codes` shares
  `create_invite_codes_body`. `get_service_auth` stays Cohttp. Does
  not invent leftover unused server query/body fields. Hosted-only
  chat / video / Tap / phone / contacts / push stay listed not faked
- [#205](https://github.com/david-engelmann/atproto/pull/205):
  Session leftover JSON XRPC via Client. `get_session_request` via
  `Client.get_text`; `refresh_session` / `delete_session` via
  `Client.post_json` with Bearer `refreshJwt`. Empty delete output
  stays `""`. Does not invent leftover unused getSession /
  refreshSession / deleteSession fields. Hosted-only chat / video /
  Tap / phone / contacts / push stay listed not faked
- [#206](https://github.com/david-engelmann/atproto/pull/206):
  Identity resolveHandle/resolveDid via Client. `resolve_handle` /
  `resolve_did` / `resolve_identity` share query-pair helpers with
  `Client.get_json`. Directory fallback stays. Does not invent
  leftover unused resolveHandle / resolveDid / resolveIdentity
  fields. Hosted-only chat / video / Tap / phone / contacts / push
  stay listed not faked
- [#207](https://github.com/david-engelmann/atproto/pull/207): Sync
  leftover JSON XRPC via Client. `get_latest_commit` /
  `get_repo_status` / `list_repos` / `list_blobs` / `list_hosts` /
  `get_host_status` / `list_repos_by_collection` share query-pair
  helpers and call `Client.get_json` (host stays `ATP_HOST` / session
  PDS, not Client's AppView default). `request_crawl` via
  `Client.post_json` keeps `string` (empty procedure output stays
  `""`). Binary `getRepo` / `getBlob` / `getBlocks` / `getRecord` CAR
  stay Cohttp. Does not invent leftover unused sync query/body fields.
  Hosted-only chat / video / Tap / phone / contacts / push stay listed
  not faked
- [#208](https://github.com/david-engelmann/atproto/pull/208):
  Server getServiceAuth via Client. `get_service_auth` via
  `Client.get_json` shares `get_service_auth_body` with
  `get_service_auth_url`. Reuses `Xrpc.service_auth_body` for aud /
  lxm validation. `service_auth` parse type and public signature
  unchanged. Does not invent leftover unused getServiceAuth fields.
  Hosted-only chat / video / Tap / phone / contacts / push stay
  listed not faked
- [#209](https://github.com/david-engelmann/atproto/pull/209): Graph
  muteActorList / muteThread Yojson body helpers
  (`mute_actor_list_body` / `unmute_actor_list_body` /
  `mute_thread_body` / `unmute_thread_body`). `mute_actor_list` /
  `unmute_actor_list` / `mute_thread` / `unmute_thread` share those
  bodies via `Client.post_json`. Public signatures stay `unit`. Does
  not invent leftover unused muteActorList / muteThread fields.
  Hosted-only chat / video / Tap / phone / contacts / push stay
  listed not faked
- [#210](https://github.com/david-engelmann/atproto/pull/210):
  Notification putPreferences v1 + putActivitySubscription Yojson
  body helpers (`put_preferences_body` /
  `put_activity_subscription_body`). `put_preferences` /
  `put_activity_subscription` share those bodies via
  `Client.post_json`. Does not invent leftover unused putPreferences
  / putActivitySubscription fields. Hosted-only chat / video / Tap /
  phone / contacts / push stay listed not faked
- [#211](https://github.com/david-engelmann/atproto/pull/211): Ozone
  `list_scheduled_actions` Yojson body helper
  (`list_scheduled_actions_body`). Lexicon fields only: required
  `statuses` (array of strings) and optional `startsAfter` /
  `endsBefore` / `subjects` / `limit` / `cursor`.
  `list_scheduled_actions` shares that body and still returns
  `scheduled_actions`. Does not invent leftover unused
  listScheduledActions fields. Hosted-only chat / video / Tap /
  phone / contacts / push stay listed not faked
- [#212](https://github.com/david-engelmann/atproto/pull/212):
  Ozone communication `updateTemplate` Yojson body helper
  (`update_template_body`). `update_template` shares that body.
  Extracts current params only (no invented `lang`). Also
  `delete_template_body` (`{ "id" }`). Does not invent leftover
  unused updateTemplate / deleteTemplate fields. Hosted-only chat /
  video / Tap / phone / contacts / push stay listed not faked
- [#213](https://github.com/david-engelmann/atproto/pull/213): Ozone
  `query_safelink_rules` Yojson body helper
  (`query_safelink_rules_body`). Lexicon fields only: optional
  `cursor` / `limit` / `urls` / `patternType` / `actions` / `reason`
  / `createdBy` / `sortDirection`. `query_safelink_rules` shares that
  body and still returns `url_rules`. Does not invent leftover unused
  queryRules fields. Hosted-only chat / video / Tap / phone /
  contacts / push stay listed not faked
- [#214](https://github.com/david-engelmann/atproto/pull/214): Ozone
  report leftover Yojson POST body helpers
  (`assign_report_moderator_body` / `reassign_queue_body` /
  `refresh_stats_body` / `close_reports_body`). Lexicon fields only:
  assignModerator `reportId` plus optional `queueId` / `did` /
  `isPermanent`; reassignQueue `reportId` / `queueId` plus optional
  `comment`; refreshStats `startDate` / `endDate` plus optional
  `queueIds`; closeReports `subject` plus optional `reportTypes` /
  `internalNote` / `isAutomated`. `assign_report_moderator` /
  `reassign_queue` / `refresh_stats` / `close_reports` share those
  bodies. Public signatures unchanged. Does not invent leftover unused
  assignModerator / reassignQueue / refreshStats / closeReports
  fields. Hosted-only chat / video / Tap / phone / contacts / push
  stay listed not faked
- [#218](https://github.com/david-engelmann/atproto/pull/218): Site typed record encodes (`document_to_json` /
  `publication_to_json` / `recommend_to_json` /
  `subscription_to_json`). Lexicon fields only; existing builders
  and parse types unchanged. Hosted-only chat / video / Tap /
  phone / contacts / push stay listed not faked
- [#215](https://github.com/david-engelmann/atproto/pull/215): Ozone
  set/setting/team leftover Yojson POST body helpers
  (`upsert_set_body` / `add_set_values_body` / `delete_set_values_body` /
  `upsert_option_body` / `update_member_body`). Lexicon fields only:
  upsertSet `name` plus optional `description`; addValues /
  deleteValues `name` / `values`; upsertOption `key` / `scope` /
  `value` plus optional `description` (does not invent leftover unused
  `managerRole`); updateMember `did` plus optional `role` / `disabled`.
  `upsert_set` / `add_set_values` / `delete_set_values` / `upsert_option` /
  `update_member` share those bodies. Public signatures unchanged.
  Does not invent leftover unused upsertSet / addValues /
  deleteValues / upsertOption / updateMember fields. Hosted-only
  chat / video / Tap / phone / contacts / push stay listed not faked
- [#216](https://github.com/david-engelmann/atproto/pull/216):
  Ozone queue leftover Yojson POST body helpers
  (`assign_queue_moderator_body` / `unassign_queue_moderator_body` /
  `route_reports_body`). `assign_queue_moderator` /
  `unassign_queue_moderator` / `route_reports` share those bodies.
  Queue assign/unassign/route only (not `report.assignModerator`).
  Does not invent leftover unused queue fields. Hosted-only chat /
  video / Tap / phone / contacts / push stay listed not faked
- [#219](https://github.com/david-engelmann/atproto/pull/219): Ozone
  `query_events` / `query_statuses` query-pair helpers
  (`query_events_body` / `query_statuses_body`). Currently sent
  fields only. `query_events` / `query_events_service` /
  `query_statuses` share those pairs. Distinct from #213 POST
  `query_safelink_rules_body`. Does not invent leftover unused
  queryEvents / queryStatuses fields. Hosted-only chat / video /
  Tap / phone / contacts / push stay listed not faked
- [#220](https://github.com/david-engelmann/atproto/pull/220): Feed
  leftover AppView query-pair helpers (`get_feed_body` /
  `get_list_feed_body` / `get_actor_feeds_body` /
  `search_posts_body`). Currently sent fields only. `get_feed` /
  `get_list_feed` / `get_actor_feeds` / `search_posts` share those
  pairs. Distinct from #202 session query-pair helpers. Does not
  invent leftover unused getFeed / getListFeed / getActorFeeds /
  searchPosts fields. Hosted-only chat / video / Tap / phone /
  contacts / push stay listed not faked
- [#221](https://github.com/david-engelmann/atproto/pull/221): Graph
  leftover AppView query-pair helpers (`get_list_body` /
  `get_lists_body` / `get_actor_starter_packs_body` /
  `search_starter_packs_body` / `get_relationships_body` /
  `get_known_followers_body`). Currently sent fields only. `get_list`
  / `get_lists` / `get_actor_starter_packs` / `search_starter_packs` /
  `search_starter_packs_v2` / `get_relationships` /
  `get_known_followers` share those pairs. Distinct from #198 session
  leftovers and `follow_page_pairs`. Does not invent leftover unused
  getList / getLists / getActorStarterPacks / searchStarterPacks /
  getRelationships / getKnownFollowers fields. Hosted-only chat /
  video / Tap / phone / contacts / push stay listed not faked
- [#222](https://github.com/david-engelmann/atproto/pull/222): Feed
  leftover searchPostsV2 / getQuotes / getActorLikes query-pair helpers
  (`search_posts_v2_body` / `get_quotes_body` / `get_actor_likes_body`).
  Currently sent fields only. `search_posts_v2` / `get_quotes` /
  `get_actor_likes` share those pairs. Siblings of #220 helpers. On
  top of #221 → #220. Does not invent leftover unused searchPostsV2 /
  getQuotes / getActorLikes fields. Hosted-only chat / video / Tap /
  phone / contacts / push stay listed not faked
- [#223](https://github.com/david-engelmann/atproto/pull/223): Unspecced
  leftover searchPostsSkeleton / searchActorsSkeleton /
  searchStarterPacksSkeleton query-pair helpers
  (`search_posts_skeleton_body` / `search_actors_skeleton_body` /
  `search_starter_packs_skeleton_body`). Currently sent fields only.
  `search_posts_skeleton` / `search_actors_skeleton` /
  `search_starter_packs_skeleton` share those pairs. Distinct from
  #220 / #222 Feed searchPosts helpers. On top of #222. Does not
  invent leftover unused searchPostsSkeleton / searchActorsSkeleton /
  searchStarterPacksSkeleton fields. Hosted-only chat / video / Tap /
  phone / contacts / push stay listed not faked
- [#224](https://github.com/david-engelmann/atproto/pull/224): Unspecced
  leftover getSuggestionsSkeleton / getPostThreadV2 / suggested-users
  query-pair helpers (`get_suggestions_skeleton_body` /
  `get_post_thread_v2_body` / `get_suggested_users_body` /
  `get_suggested_users_skeleton_body`). Currently sent fields only.
  `get_suggestions_skeleton` / `get_post_thread_v2` /
  `get_suggested_users` / `get_suggested_onboarding_users` /
  `get_suggested_users_for_explore` /
  `get_suggested_users_for_see_more` / `get_suggested_users_skeleton`
  / `get_onboarding_suggested_users_skeleton` /
  `get_suggested_users_for_explore_skeleton` /
  `get_suggested_users_for_see_more_skeleton` share those pairs.
  Distinct from #223 search skeleton helpers. On top of #223. Does not
  invent leftover unused getSuggestionsSkeleton / getPostThreadV2 /
  getSuggestedUsers fields. Hosted-only chat / video / Tap / phone /
  contacts / push stay listed not faked
- [#225](https://github.com/david-engelmann/atproto/pull/225): Ozone
  leftover query_reports / search_repos query-pair helpers
  (`query_reports_body` / `search_repos_body`). Currently sent
  fields only. `query_reports` / `search_repos` share those pairs.
  Distinct from #219 `query_events_body` / `query_statuses_body`
  and #214 report POST leftovers. On top of #224. Does not invent
  leftover unused queryReports / searchRepos fields. Hosted-only
  chat / video / Tap / phone / contacts / push stay listed not faked
- [#226](https://github.com/david-engelmann/atproto/pull/226):
  Notification leftover list_notifications query-pair helpers
  (`list_notifications_body`). Currently sent fields only.
  `list_notifications` / `list_notifications_page` share those pairs.
  Distinct from #210 putPreferences / putActivitySubscription POST
  leftovers and #203 getUnreadCount / updateSeen. On top of #225.
  Does not invent leftover unused listNotifications fields.
  Hosted-only chat / video / Tap / phone / contacts / push stay
  listed not faked
- [#227](https://github.com/david-engelmann/atproto/pull/227): Feed
  leftover getAuthorFeed / getFeedSkeleton query-pair helpers
  (`get_author_feed_body` / `get_feed_skeleton_body`). Currently sent
  fields only. `get_author_feed` / `get_author_feed_page` /
  `get_feed_skeleton` / `get_feed_skeleton_parsed` share those pairs.
  Distinct from #202 session leftovers and #220 / #222 AppView
  leftovers. On top of #226. Does not invent leftover unused
  getAuthorFeed / getFeedSkeleton fields. Hosted-only chat / video /
  Tap / phone / contacts / push stay listed not faked
- [#228](https://github.com/david-engelmann/atproto/pull/228):
  opam-repository publish prep. `dune-project` package description /
  generated `atproto.opam` / README install + Remaining gaps /
  CONTRIBUTING / `doc/index.mld` no longer say the package is not on
  opam-repository. `opam install atproto` after the opam-repository
  PR merges; GitHub pin still works for development. Hosted-only
  chat / video / Tap / phone / contacts / push stay listed not faked.
  No lexicon pin bump
- [#229](https://github.com/david-engelmann/atproto/pull/229):
  OCaml 5 / packaging modernization hop toward 1.0.0. Lift the
  hard `ocaml < 5.0` ceiling to `>= 4.14.1` and `< 5.4`. Pin Jane
  Street `core` / `async` / `ppx_jane` / `zstandard` to
  `>= v0.16.0` and `< v0.18~` so 4.14 resolves v0.16 and OCaml
  5.1–5.3 resolves v0.17 (the 5-ready line). CI `build` matrix is
  4.14.1 + 5.3.0; `lint-*` and `local-pds` stay on 4.14.1.
  ocamlformat stays **0.25.1** (`lint-fmt` on 4.14.1; that release
  needs OCaml `< 5.2`). Bump dune lang to 3.11 (Jane Street v0.17
  requires dune `>= 3.11`). Package version stays `0.1.0` (do not
  retag). Hosted-only chat / video / Tap / phone / contacts / push
  stay listed not faked. No lexicon pin bump
- Public HTTPS OAuth client-metadata + production browser-login
  path. `https_client_id` / `public_https_metadata` /
  `validate_https_metadata` enforce AT Protocol HTTPS `client_id`
  (no port / query / fragment) and redirect rules (web HTTPS
  same-origin; native loopback `127.0.0.1` / `[::1]` or
  reverse-domain custom scheme). `metadata_document` /
  `metadata_http_headers` / `metadata_http_response` serialize an
  HTTP 200 `application/json` body for the app to host.
  `fetch_client_metadata` requires status 200, JSON content-type,
  and an exact `client_id` match. `start_browser_login` /
  `complete_browser_login` discover the PDS AS, PAR, build the
  authorize URL, then parse the redirect and exchange the code.
  `examples/client-metadata.json` + `examples/oauth_https_metadata.ml`
  are offline scaffolding, not a hosted login UI. Local TestNetwork
  loopback metadata stays valid. Hosted-only chat / video / Tap /
  phone / contacts / push stay listed not faked. No lexicon pin bump
- `examples/offline.ml` typechecks against the public API under
  `dune build` / `dune runtest`

### Not in this release

- An application still has to host the HTTPS `client-metadata.json`
  and receive the browser redirect (this library builds/validates
  the document and drives authorize → code → token; it does not
  host them or a login UI)
- Hosted Tap service or video transcoder
- Official OSS chat backend (TestNetwork does not start one)
- Newly published official lexicons after bluesky-social/atproto
  `f0d4877a` — the coverage gate fails until the pin
  snapshot and bindings (or an explicit skip) are updated
- Jetstream archive HTTP download still needs an operator token this
  library does not invent (live compressed `subscribeEvents` and
  `xrpc.v1.json` subprotocol negotiation are implemented)
