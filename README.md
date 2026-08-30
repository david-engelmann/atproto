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

Session creation, repo writes, graph, and feed helpers need `ATP_AUTH`. Public identity, DID PLC, and most `com.atproto.sync.*` reads do **not**.

## Build and test

```shell
opam install . --deps-only --with-test
dune build
dune runtest
```

Live Bluesky tests that need credentials are skipped unless `ATP_AUTH` is set to a real `email:app-password` pair (placeholder values in `sample.env` do not count). Public-network tests (handle resolve, PLC directory, `getLatestCommit`) run without auth and skip only if the request itself fails.

## What this library covers

| Area | Module | Notes |
| --- | --- | --- |
| Session / JWT | `Auth`, `Session` | `createSession` URL uses `ATP_HOST` + `BASE_ENDPOINT` |
| AppView | `Actor`, `Feed`, `Graph`, `Notification` | Profiles, search (`q`), follows/blocks/mutes |
| Repo writes | `Repo` | `createRecord` / `putRecord` send `record` as JSON |
| Server | `Server` | describe server, app passwords, invites |
| Identity | `Identity`, `Did_plc` | `resolveHandle`, `did:plc` documents via `plc.directory` |
| Sync | `Sync` | Current `getLatestCommit`, `getRepo` (CAR), `listBlobs`, `listRepos` |
| CID / CAR | `Cid`, `Car`, `Dag_cbor` | CIDv1 + CARv1 parse/encode |
| AT URI | `At_uri` | `at://` parse / serialize |
| Lexicon | `Lexicon` | Parse lexicon-1 JSON documents |
| Firehose | `Firehose` | Decode `subscribeRepos` DAG-CBOR frames (no WebSocket client yet) |
| Errors | `Error` | XRPC `{error, message}` including rate limits |

## Remaining gaps

- Firehose **WebSocket** subscribe client (`wss://bsky.network/xrpc/com.atproto.sync.subscribeRepos`)
- MST verification / inductive firehose inversion
- Lexicon codegen (types are parsed, not generated)
- OAuth / DPoP (app-password sessions only)
- Full PLC operation-chain signature verification
- `did:web` resolution

Open PR `#69` (`de-sync-types`) is superseded by this work: it still targeted the removed `getCheckout` API and left CAR/CBOR unfinished.

## Sample usage

```ocaml
(* public, no auth *)
let did = (Identity.resolve_handle "jay.bsky.team").did
let commit = Sync.get_latest_commit did
let doc = Did_plc.resolve did

(* authenticated writes *)
let username, password = Auth.username_and_password_from_env
let session = Session.create_session username password
let profile = Actor.get_profile session "jay.bsky.team"
```
