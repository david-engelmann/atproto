# Contributing

Pull requests are welcome. **1.0.1** is tagged; package version on
this tree is **1.0.2**. The public opam package is
[ocaml/opam-repository#30703](https://github.com/ocaml/opam-repository/pull/30703)
(`opam install atproto` after that merges; a 1.0.2 publish
supersedes it). Pin the GitHub repository for development. Product
docs for third-party users are in [README.md](../README.md) and
https://david-engelmann.github.io/atproto/.

## Toolchain

- OCaml **>= 4.14.1 and < 5.4**. CI `build` tests **4.14.1** and **5.3.0**.
  Jane Street `core` / `async` / `ppx_jane` / `zstandard` are
  `>= v0.16.0` and `< v0.18~` (v0.16 on 4.14, v0.17 on 5.1–5.3).
  Public Jane Street v0.17 does not support OCaml 5.4+.
- ocamlformat **0.25.1** (see `.ocamlformat`). That release needs
  OCaml `< 5.2`, so `lint-fmt` stays on **4.14.1**. `lint-fmt` must
  stay green. A 0.27+ bump (and reformat) is a later hop.

- System libzstd (Jane Street `zstandard` / Jetstream dict-zstd):
  Ubuntu/Debian `libzstd-dev`, macOS Homebrew `zstd` (headers ship
  with the formula). Required, not optional.
- Package-style build (what `opam install` / a dependent sees):
  `dune build -p atproto` and `dune runtest -p atproto` (unit tests;
  offline unless `ATP_PUBLIC=1`). Official NSID coverage is `dune
  build @lexicon-coverage` (CI-only; not `@runtest` / opam
  `with-test`).

Do not hand-edit `atproto.opam`; it is generated from `dune-project`.
`dune-project` keeps `(version ...)`. The generated in-repo opam file
includes `version:`. When submitting to ocaml/opam-repository, omit
that redundant `version:` field (`opam lint`).
odoc HTML is a CI artifact (`odoc-html`) on pull requests. On push to
`main`, TestSuite deploys `_build/default/_doc/_html` with GitHub
Actions Pages. GitHub Pages is enabled (Settings → Pages → Source:
GitHub Actions). The live site is
https://david-engelmann.github.io/atproto/. `dune-project`
`documentation` points at that URL. Build odoc locally with
`make doc`.

## Checks

CI jobs: `build`, `lint-doc`, `lint-fmt`, `lint-opam`, `local-pds`.
Public-internet hops are the optional **PublicLive** workflow
(`workflow_dispatch`, `ATP_PUBLIC=1`); they are not a required
TestSuite / merge-when-green check.

On push to `main`, `deploy-pages` publishes odoc HTML to
https://david-engelmann.github.io/atproto/.

Open, non-draft pull requests that target `main` from this repository
(or Dependabot) may be **squash-merged automatically** by
`.github/workflows/merge-when-green.yml` once CI is green; the head
branch is deleted. Docs-only diffs (`CHANGELOG` / `README` / `doc/**` /
`*.md` / `.github` markdown, issue templates, `CODEOWNERS`,
`dependabot.yml`) merge after `lint-fmt` and `lint-doc` succeed.
`.github/scripts/**`, workflow YAML, and other `.github` files wait for
full TestSuite. Other PRs wait for TestSuite `build`, `local-pds`,
and `lint-*`. Forks, drafts, and failing checks are never merged.
`lexicon-pin` drift does not block merge-when-green; pin bumps are
separate PRs.
Stacked PRs that are only behind `main` get an update-branch after a
merge. "Allow auto-merge" in repo settings is optional — the workflow
merges on green itself. Branch protection that requires a human review
will block the Actions token; merge those PRs manually or do not
require a review for this automation.

## Local TestNetwork

`make test-pds` starts official `@atproto/dev-env@0.6.4` (PLC, PDS,
AppView, Ozone). Chat, video, Tap, SMS, and push are not in that
stack — do not stub them. OAuth against the local AS (loopback
metadata, PAR, DPoP, CSRF cookies, `getServiceAuth` for AppView /
Ozone) lives in `test/test_local_oauth.ml`; do not invent a CSRF
token. Authenticated AppView / Ozone reject a DPoP access token and
a `createSession` `at+jwt` — mint service-auth instead. DPoP cannot
be proxied.

CI installs Ubuntu `libzstd-dev` before every OCaml job; install
that or Homebrew `zstd` before the commands below.

```shell
opam install . --deps-only --with-test
# formatter is not a package dep (0.25.1 needs OCaml < 5.2)
opam install ocamlformat.0.25.1
dune build -p atproto
dune runtest -p atproto
dune build @lexicon-coverage
opam lint atproto.opam
```

`dune build` also typechecks `examples/offline.ml` against the public API.

## Lexicon coverage

Official lexicons are pinned at bluesky-social/atproto
[`f0d4877a03`](https://github.com/bluesky-social/atproto/commit/f0d4877a03dc8ede0d3e9a36d5b72ada63b5d2e0).
`scripts/gen-official-nsids.py` rebuilds
`lexicons/official-nsids.json` against a SHA. TestSuite
`@lexicon-coverage` (`test_lexicon_coverage`) fails if a public client
NSID is missing a helper, record builder, bundled permission-set, or
an explicit one-line skip. opam `with-test` / `@runtest` run unit
tests only; the coverage gate is CI-only.

Five deprecated/internal NSIDs are skipped in
`lexicons/coverage-skips.json`: `com.atproto.temp.fetchLabels`,
`com.atproto.sync.getCheckout`, `com.atproto.sync.getHead`,
`com.atproto.sync.notifyOfUpdate`, and `internal.bsky.actor.getProfiles`.
Hosted-only servers (no OSS chat backend, no video transcoder, no Tap
host, no SMS gateway, no APNs/FCM push backend) are not skip reasons.
