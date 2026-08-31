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
| Session / JWT | `Auth`, `Session` | `createSession` URL uses `ATP_HOST` + `BASE_ENDPOINT` |
| AppView | `Actor`, `Feed`, `Graph`, `Notification` | Profiles, search (`q`), follows/blocks/mutes |
| Repo writes | `Repo` | `createRecord` / `putRecord` / `deleteRecord` / `applyWrites` bodies; `uploadBlob` parse |
| Server | `Server` | describe server, app passwords, invites |
| Identity | `Identity`, `Did_plc`, `Did_web`, `Did_key` | `resolveHandle`, `did:plc`, `did:web` (including `%3A` ports), `did:key` |
| PLC chain | `Did_plc` | Genesis DID, prev CID links, p256 **and k256** ECDSA (low-S, IEEE P1363) |
| Sync | `Sync` | Current `getLatestCommit`, `getRepo` (CAR), `listBlobs`, `listRepos` |
| CID / CAR | `Cid`, `Car`, `Dag_cbor` | CIDv1 (including SHA-256 `Cid.create`) + CARv1 |
| MST | `Mst` | Layer/prefix rules, node parse, CID verify, lookup, insert/delete, firehose-diff inversion, p256/k256 commit sign+verify |
| TID | `Tid` | Record-key / commit-rev identifiers (base32-sortable, official syntax) |
| AT URI | `At_uri` | `at://` parse / serialize |
| Lexicon | `Lexicon` | Parse lexicon-1 JSON, `to_ocaml` codegen, JSON validate |
| Firehose | `Firehose`, `Websocket` | RFC 6455 client + `subscribeRepos` frame decode (`#commit`/`#sync`/`#identity`/`#account`/`#info`) |
| OAuth / DPoP | `Oauth` | PKCE S256, DPoP ES256 + nonce, client metadata, AS/resource metadata, redirect callback, PAR/token loop (injectable HTTP) |
| Labels | `Label` | `queryLabels` + label / query parse (`ver`, `exp`) |
| Errors | `Error` | XRPC `{error, message}` including rate limits |

## Remaining gaps

These are product-level, not missing protocol cores:

- Hosting a public **client-metadata document** and completing a **live browser login** against a PDS (the protocol core — metadata, PAR + DPoP-nonce retry, authorize URL, redirect `code`/`state`/`iss`, token parse — is implemented and tested with fixtures)

PLC k256 verify, MST firehose-diff inversion, signed-commit verify, and OAuth client-metadata / PAR+token loop are implemented in this stack (#70 / #71 / this slice).

Open PR `#69` (`de-sync-types`) is superseded by this work: it still targeted the removed `getCheckout` API and left CAR/CBOR unfinished.

## Sample usage

```ocaml
(* public, no auth *)
let did = (Identity.resolve_handle "jay.bsky.team").did
let commit = Sync.get_latest_commit did
let doc = Did_plc.resolve did
let ident = Identity.resolve "jay.bsky.team"

(* MST layer for a repo key — official vector *)
let () = assert (Mst.layer_for_key "blue" = 1)

(* TID used as record keys and commit revs *)
let () = assert (Tid.is_valid "3jzfcijpj2z2a")

(* OAuth client metadata + authorize URL (no hosted client required) *)
let meta =
  Oauth.public_metadata
    ~client_id:"https://client.example/client-metadata.json"
    ~redirect_uris:[ "https://client.example/cb" ] ()
let _ = Oauth.validate_metadata meta

(* firehose: one subscribeRepos frame from the public relay *)
let _header, msg = Firehose.subscribe_one ()

(* authenticated writes *)
let username, password = Auth.username_and_password_from_env
let session = Session.create_session username password
let profile = Actor.get_profile session "jay.bsky.team"
```
