#!/usr/bin/env bash
# Install Ubuntu libzstd-dev for Jane Street zstandard / Jetstream dict-zstd.
# GitHub ubuntu runners ship third-party apt sources (Google Chrome
# .list/.sources, Microsoft packages). Those indexes flake (Hash Sum
# mismatch, 403 Forbidden / "no longer signed") and must not fail
# required CI. libzstd-dev is in Ubuntu main.
set -euo pipefail

third_party_sources="$(
  grep -lRE 'dl.google.com|packages.microsoft.com' \
    /etc/apt/sources.list /etc/apt/sources.list.d 2>/dev/null || true
)"
if [ -n "${third_party_sources}" ]; then
  # shellcheck disable=SC2086
  sudo rm -f ${third_party_sources}
fi
sudo rm -f /var/lib/apt/lists/*chrome* /var/lib/apt/lists/*google* \
  /var/lib/apt/lists/*microsoft* || true
sudo apt-get update
sudo apt-get install -y libzstd-dev
