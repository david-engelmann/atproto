---
name: Install
about: Pin, opam, or dune build problems
title: ""
labels: ""
---

**What failed**
`opam pin`, `opam install . --deps-only`, or `dune build -p atproto`.

**Environment**
- OCaml version (must be `>= 4.14.1` and `< 5.0`; CI is 4.14.1):
- opam version / switch:
- OS:

**Command and output**
Paste the command and the error.

This package is not on the public opam-repository. The supported install is:

```shell
opam pin add atproto git+https://github.com/david-engelmann/atproto.git
```
