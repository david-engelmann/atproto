# atproto

OCaml toolkit for the [AT Protocol](https://atproto.com) (XRPC, lexicons, repo sync, identity).

## Environment

Create a `.env` (see `sample.env`) with at least:

- `ATP_AUTH` : `EmailAddress:AppPassword`
  - Use an [App Password](https://bsky.app/settings/app-passwords) (email as the username).
- `ATP_HOST` : `bsky.social`
  - PDS / entryway host **without** a scheme.

Optional:

- `BASE_ENDPOINT` : `xrpc` (default)

Session creation, repo writes, graph, and feed helpers need `ATP_AUTH`. Public identity, DID PLC, firehose subscribe, and most `com.atproto.sync.*` reads do **not**.

## Build and test

```shell
opam install . --deps-only --with-test
dune build
dune runtest
```

Live Bluesky tests that need credentials are skipped unless `ATP_AUTH` is set to a real `email:app-password` pair (placeholder values in `sample.env` do not count). Public-network tests (handle resolve, PLC directory, `getLatestCommit`, `subscribeRepos`) run without auth and skip only if the request itself fails.

## What this library covers

| Area | Module | Notes |
| --- | --- | --- |
| Syntax | `Syntax` | Handle, DID, NSID, record key, datetime, language, at-identifier |
| Session / JWT | `Auth`, `Session` | `createSession` URL uses `ATP_HOST` + `BASE_ENDPOINT` |
| AppView | `Actor`, `Feed`, `Graph`, `Notification` | Profiles, search (`q`), follows/blocks/mutes |
| Repo writes | `Repo` | `createRecord` / `putRecord` / `applyWrites` / `uploadBlob` / `listMissingBlobs` / `importRepo` |
| Server | `Server` | describe server, app passwords, invites, `getServiceAuth`, account status |
| Identity | `Identity`, `Did_plc`, `Did_web`, `Did_key` | `resolveHandle` / `resolveIdentity`, `did:plc`, `did:web`, `did:key` |
| PLC chain | `Did_plc` | Genesis DID, prev CID links, p256 + k256 ECDSA (low-S, IEEE P1363) |
| Sync | `Sync` | `getLatestCommit`, `getRepo` (CAR), `listBlobs`, `listRepos`, `getRepoStatus`, `listHosts`, `listReposByCollection` |
| CID / CAR | `Cid`, `Car`, `Dag_cbor` | CIDv1 (including SHA-256 `Cid.create`) + CARv1 |
| MST | `Mst` | Layer/prefix rules, node parse, CID verify, lookup, insert/delete, firehose-diff invert |
| AT URI | `At_uri` | `at://` parse / serialize |
| Lexicon | `Lexicon` | Parse lexicon-1 JSON, `to_ocaml` codegen, JSON validate |
| Firehose | `Firehose`, `Websocket` | RFC 6455 client + `subscribeRepos` frame decode + commit verify |
| Labels | `Label` | `queryLabels` parse, signed labels (p256/k256), `subscribeLabels` frames |
| XRPC | `Xrpc`, `Error` | `atproto-proxy`, accept/content-labelers, rate-limit headers, service-auth JWT |
| OAuth / DPoP | `Oauth` | PKCE S256, DPoP ES256, PAR/token request shapes |
| TID | `Tid` | Timestamp identifiers |
| Signed commits | `Mst` | Repo commit sign/verify (p256/k256) |

## Remaining gaps

The protocol core is complete. Leftovers are product / application-level:

- OAuth **browser redirect / client-metadata hosting / live token loop** against a PDS (PKCE + DPoP + PAR encoding and ES256 proofs are implemented)
- Hosting a public client-metadata URL or completing a live browser login
- AppView product surfaces beyond the existing profile / feed / graph / notification helpers
- Admin / ozone moderation dashboards (createReport + labeler proxy headers are implemented)
- Live `getServiceAuth` against a real PDS (JWT parse and request shape are implemented; skipped without `ATP_AUTH`)

## Sample usage

```ocaml
(* public, no auth *)
let did = (Identity.resolve_handle "jay.bsky.team").did
let commit = Sync.get_latest_commit did
let doc = Did_plc.resolve did
let ident = Identity.resolve "jay.bsky.team"

(* syntax — official spec vectors *)
let () = assert (Syntax.is_valid_nsid "com.atproto.sync.getRecord")
let () = assert (Syntax.is_valid_handle "jay.bsky.team")
let () = assert (Syntax.is_valid_datetime "1985-04-12T23:20:50.123Z")

(* MST layer for a repo key — official vector *)
let () = assert (Mst.layer_for_key "blue" = 1)

(* firehose: one subscribeRepos frame from the public relay *)
let _header, msg = Firehose.subscribe_one ()

(* authenticated writes *)
let username, password = Auth.username_and_password_from_env
let session = Session.create_session username password
let profile = Actor.get_profile session "jay.bsky.team"
```
