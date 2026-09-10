# Agents

This repository is an OCaml **client and protocol library** for the [AT Protocol](https://atproto.com). Read [README.md](README.md) first; module HTML is at https://david-engelmann.github.io/atproto/.

## Install and call

- Package: `atproto`. Dune: `(libraries atproto)`.
- Packaged **1.0.2**. Until **atproto.1.0.2** is on opam-repository, `opam pin add atproto git+https://github.com/david-engelmann/atproto.git`. Then `opam install atproto`.
- OCaml `>= 4.14.1` and `< 5.4`. System **libzstd** is required (Jetstream dict-zstd).
- Public live tests skip unless `ATP_PUBLIC=1`. Default `opam install -t` / `@runtest` is offline. Credential hops stay on `ATP_AUTH`; local TestNetwork stays on `ATP_LOCAL_PDS`.
- Public, unauthenticated starting point:

```ocaml
let did = (Identity.resolve_handle "jay.bsky.team").did
let posts = Feed.search_posts ~q:"atproto" ~limit:5 ()
```

## Honesty constraints

Do not invent or stub hosted products. Do not claim public-network
tests run in default `with-test` (they need `ATP_PUBLIC=1`). This
package does **not** provide:

- a PDS, OSS chat backend, video transcoder, Tap host, SMS gateway, or APNs/FCM
- a Jetstream archive API key (operator supplies `JETSTREAM_API_KEY`)
- a space host

`Chat`, `Video`, `Contact`, `Notification.register_push`, and `Repo_sync` are **clients** (or an indexer toolkit). OAuth still requires the application to host `client-metadata.json`.

`Lt_hash`, `At_uri.Space`, `Space_commit`, `Space_credential`, `Space_xrpc`, and `Space_sync` are **experimental** proposal [0016](https://github.com/bluesky-social/proposals/blob/main/0016-permissioned-data/README.md) helpers — not a product API. Live space hops skip unless `ATP_SPACE=1` and `ATP_SPACE_HOST` names a real host.

Official lexicons are pinned at `f0d4877a`. Do not bump that pin in a docs change.

## Where to look

| Question | Source |
| --- | --- |
| What to install / first call | [README.md](README.md) |
| What each module is for | README library map + [odoc](https://david-engelmann.github.io/atproto/) |
| What shipped when | [CHANGELOG.md](CHANGELOG.md) |
| How to contribute / local TestNetwork | [.github/CONTRIBUTING.md](.github/CONTRIBUTING.md) |
| Env names | `sample.env` |

Prefer those documents over CI logs, helper-name dumps, or reconstructing the API from pull-request titles.
