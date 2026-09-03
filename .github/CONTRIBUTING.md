# Contributing

Pull requests are welcome. This package is **not** on
[opam-repository](https://github.com/ocaml/opam-repository); depend on it by
pinning the GitHub repository.

## Toolchain

- OCaml **4.14.x only** (`>= 4.14.1` and `< 5.0`). CI tests **4.14.1**.
- ocamlformat **0.25.1** (see `.ocamlformat`). `lint-fmt` must stay green.
- System libzstd (Jane Street `zstandard` / Jetstream dict-zstd):
  Ubuntu/Debian `libzstd-dev`, macOS Homebrew `zstd` (headers ship
  with the formula). Required, not optional.
- Package-style build (what `opam install` / a dependent sees):
  `dune build -p atproto` and `dune runtest -p atproto`.

Do not hand-edit `atproto.opam`; it is generated from `dune-project`.
odoc HTML is a CI artifact (`odoc-html`) on pull requests. On push to
`main`, TestSuite deploys `_build/default/_doc/_html` with GitHub
Actions Pages. GitHub Pages is enabled (Settings → Pages → Source:
GitHub Actions). The live site is
https://david-engelmann.github.io/atproto/. `dune-project`
`documentation` points at that URL. Build odoc locally with
`make doc`.

## Checks

CI jobs: `build`, `lint-doc`, `lint-fmt`, `lint-opam`, `local-pds`.
On push to `main`, `deploy-pages` publishes odoc HTML to
https://david-engelmann.github.io/atproto/.
CI installs Ubuntu `libzstd-dev` before every OCaml job; install
that or Homebrew `zstd` before the commands below.

```shell
opam install . --deps-only --with-test
dune build -p atproto
dune runtest -p atproto
opam lint atproto.opam
```

`dune build` also typechecks `examples/offline.ml` against the public API.

## Lexicon coverage

Official lexicons are pinned at bluesky-social/atproto
[`5c154f9c51`](https://github.com/bluesky-social/atproto/commit/5c154f9c5173e7823a5353eab92207508a7dea99).
`scripts/gen-official-nsids.py` rebuilds
`lexicons/official-nsids.json` against a SHA. TestSuite
`test_lexicon_coverage` fails if a public client NSID is missing a helper,
record builder, bundled permission-set, or an explicit one-line skip.

Five deprecated/internal NSIDs are skipped in
`lexicons/coverage-skips.json`: `com.atproto.temp.fetchLabels`,
`com.atproto.sync.getCheckout`, `com.atproto.sync.getHead`,
`com.atproto.sync.notifyOfUpdate`, and `internal.bsky.actor.getProfiles`.
Hosted-only servers (no OSS chat backend, no video transcoder, no Tap
host) are not skip reasons.
