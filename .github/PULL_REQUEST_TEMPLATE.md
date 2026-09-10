## Summary

<!-- What changed and why. Note any backwards-incompatible API shifts. -->

## Checklist

- [ ] Targets supported OCaml (`>= 4.14.1` and `< 5.4`; CI is 4.14.1 + 5.3.0)
- [ ] Not an opam-repository publish
- [ ] No lexicon pin bump unless `scripts/gen-official-nsids.py` was re-run and `@lexicon-coverage` (`test_lexicon_coverage`, or an explicit skip) still passes
- [ ] No fake OSS chat / video transcoder / Tap host / SMS gateway / APNs-FCM push backend
- [ ] No invented Jetstream archive token
- [ ] New public module has a module-level `(** ... *)` odoc comment
- [ ] User-facing change is noted in CHANGELOG.md

Fixes # (issue)
