#!/usr/bin/env bash
# Install Ubuntu libzstd-dev for Jane Street zstandard / Jetstream dict-zstd.
# GitHub ubuntu runners ship Google Chrome apt sources (.list and DEB822
# .sources). Those third-party indexes flake with Hash Sum mismatch and
# must not fail required CI. libzstd-dev is in Ubuntu main.
set -euo pipefail

chrome_sources="$(grep -lR 'dl.google.com' /etc/apt/sources.list /etc/apt/sources.list.d 2>/dev/null || true)"
if [ -n "${chrome_sources}" ]; then
  # shellcheck disable=SC2086
  sudo rm -f ${chrome_sources}
fi
sudo rm -f /var/lib/apt/lists/*chrome* /var/lib/apt/lists/*google* || true
sudo apt-get update
sudo apt-get install -y libzstd-dev
