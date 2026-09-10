---
name: Install
about: Pin, opam, or dune build problems
title: ""
labels: ""
---

**What failed**
`opam install atproto`, `opam pin`, `opam install . --deps-only`, or `dune build -p atproto`.

**Environment**
- OCaml version (must be `>= 4.14.1` and `< 5.4`; CI is 4.14.1 and 5.3.0):
- opam version / switch:
- OS:
- System libzstd (Jane Street `zstandard` / Jetstream dict-zstd; Ubuntu/Debian `libzstd-dev`, macOS Homebrew `zstd`):

**Command and output**
Paste the command and the error.

**1.0.2** is the packaged surface. `opam install atproto` after
**atproto.1.0.2** lands on opam-repository. Until then, pin this
repository:

```shell
opam install atproto
# development pin
opam pin add atproto git+https://github.com/david-engelmann/atproto.git
```

Jetstream dict-zstd needs Jane Street `zstandard` / system libzstd (Ubuntu/Debian `libzstd-dev`, macOS Homebrew `zstd`) before `opam install` / `opam pin`.
