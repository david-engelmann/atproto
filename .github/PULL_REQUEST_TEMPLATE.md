## Summary

<!-- What changed and why. Note any backwards-incompatible API shifts. -->

## Checklist

- [ ] Targets OCaml **4.14** only (`>= 4.14.1` and `< 5.0`; CI is 4.14.1)
- [ ] Not an opam-repository publish
- [ ] No lexicon pin bump unless `scripts/gen-official-nsids.py` was re-run and `test_lexicon_coverage` (or an explicit skip) still passes
- [ ] No fake OSS chat / video transcoder / Tap host
- [ ] No invented Jetstream archive token
- [ ] New public module has a module-level `(** ... *)` odoc comment
- [ ] User-facing change is noted in CHANGELOG.md

Fixes # (issue)
