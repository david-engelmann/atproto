#!/usr/bin/env bash
# After `dune build @doc`, replace the stock package-list index with an
# immediate redirect to the atproto landing (doc/index.mld).
#
# dune marks generated HTML 0444. Do not truncate dest in place (`>` /
# `cat >dest` fails with Permission denied). Write beside, then replace.
set -euo pipefail

root="${1:-_build/default/_doc/_html}"
target="${root}/atproto/index.html"
dest="${root}/index.html"

if [[ ! -f "${target}" ]]; then
  echo "missing package docs at ${target}; run dune build @doc first" >&2
  exit 1
fi

tmp="${dest}.new.$$"
cleanup() { rm -f "${tmp}"; }
trap cleanup EXIT

cat >"${tmp}" <<'HTML'
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta http-equiv="refresh" content="0; url=atproto/index.html">
  <link rel="canonical" href="atproto/index.html">
  <title>atproto</title>
</head>
<body>
  <p><a href="atproto/index.html">atproto package documentation</a></p>
</body>
</html>
HTML

if [[ -e "${dest}" ]]; then
  chmod u+w "${dest}" 2>/dev/null || true
  rm -f "${dest}"
fi
mv -f "${tmp}" "${dest}"
trap - EXIT

if ! grep -q 'url=atproto/index.html' "${dest}"; then
  echo "root index.html is missing meta-refresh to atproto/index.html" >&2
  exit 1
fi
if ! grep -q 'href="atproto/index.html"' "${dest}"; then
  echo "root index.html is missing fallback link to atproto/index.html" >&2
  exit 1
fi
