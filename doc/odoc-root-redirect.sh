#!/usr/bin/env bash
# After `dune build @doc`, replace the stock package-list index with an
# immediate redirect to the atproto landing (doc/index.mld).
set -euo pipefail

root="${1:-_build/default/_doc/_html}"
target="${root}/atproto/index.html"
dest="${root}/index.html"

if [[ ! -f "${target}" ]]; then
  echo "missing package docs at ${target}; run dune build @doc first" >&2
  exit 1
fi

cat >"${dest}" <<'HTML'
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
