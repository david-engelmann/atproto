#!/usr/bin/env python3
"""Generate lexicons/official-nsids.json from bluesky-social/atproto.

Re-run against a SHA (default: the APP-2933 pin) after official lexicon JSON
changes. This writes a compact NSID + main-def-type snapshot, not the 403
lexicon JSON bodies.

Usage:
  scripts/gen-official-nsids.py
  scripts/gen-official-nsids.py 60c4395951
  scripts/gen-official-nsids.py 60c4395951 /path/to/atproto/lexicons
"""

from __future__ import annotations

import json
import sys
import tarfile
import tempfile
import urllib.request
from pathlib import Path

DEFAULT_SHA = "60c439595101fbcbe612463e6f23200590c5daaf"
MAIN_TYPES = ("query", "procedure", "subscription", "record", "permission-set")
TARBALL_URL = "https://codeload.github.com/bluesky-social/atproto/tar.gz/{sha}"


def repo_root() -> Path:
    return Path(__file__).resolve().parent.parent


def load_lexicon_dir(lexicon_dir: Path) -> tuple[list[dict], int]:
    files = sorted(lexicon_dir.rglob("*.json"))
    entries: list[dict] = []
    for path in files:
        data = json.loads(path.read_text(encoding="utf-8"))
        nsid = data.get("id")
        main = (data.get("defs") or {}).get("main")
        if not isinstance(nsid, str) or not isinstance(main, dict):
            continue
        kind = main.get("type")
        if kind in MAIN_TYPES:
            entries.append({"id": nsid, "type": kind})
    entries.sort(key=lambda e: (e["id"], e["type"]))
    return entries, len(files)


def fetch_lexicon_dir(sha: str, dest: Path) -> Path:
    url = TARBALL_URL.format(sha=sha)
    tarball = dest / "atproto.tar.gz"
    with urllib.request.urlopen(url) as resp, tarball.open("wb") as out:
        out.write(resp.read())
    with tarfile.open(tarball, "r:gz") as tar:
        tar.extractall(dest)
    matches = list(dest.glob("*/lexicons"))
    if not matches:
        raise SystemExit(f"no lexicons/ directory in tarball for {sha}")
    return matches[0]


def write_manifest(sha: str, lexicon_dir: Path, out: Path) -> None:
    entries, json_count = load_lexicon_dir(lexicon_dir)
    counts: dict[str, int] = {t: 0 for t in MAIN_TYPES}
    for e in entries:
        counts[e["type"]] += 1
    manifest = {
        "source": "https://github.com/bluesky-social/atproto",
        "sha": sha,
        "main_types": list(MAIN_TYPES),
        "lexicon_json_count": json_count,
        "nsid_count": len(entries),
        "counts": counts,
        "nsids": entries,
    }
    out.parent.mkdir(parents=True, exist_ok=True)
    out.write_text(json.dumps(manifest, indent=2) + "\n", encoding="utf-8")
    print(
        f"wrote {out} ({len(entries)} NSIDs from {json_count} lexicon JSON files at {sha})"
    )


def main(argv: list[str]) -> int:
    sha = argv[1] if len(argv) > 1 else DEFAULT_SHA
    out = repo_root() / "lexicons" / "official-nsids.json"
    if len(argv) > 2:
        lexicon_dir = Path(argv[2]).resolve()
        if lexicon_dir.name != "lexicons":
            candidate = lexicon_dir / "lexicons"
            if candidate.is_dir():
                lexicon_dir = candidate
        if not lexicon_dir.is_dir():
            raise SystemExit(f"lexicon tree not found: {argv[2]}")
        write_manifest(sha, lexicon_dir, out)
        return 0
    with tempfile.TemporaryDirectory(prefix="atproto-lexicons-") as tmp:
        lexicon_dir = fetch_lexicon_dir(sha, Path(tmp))
        write_manifest(sha, lexicon_dir, out)
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
