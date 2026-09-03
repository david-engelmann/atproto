#!/usr/bin/env bash
# Squash-merge open, non-draft PRs targeting main once CI is green.
# Intended to run from GitHub Actions on the default branch (trusted).
set -euo pipefail

REPO="${GITHUB_REPOSITORY:-}"
SELF_WORKFLOW_NAME="${SELF_WORKFLOW_NAME:-Merge when green}"
EVENT_NAME="${EVENT_NAME:-}"
WORKFLOW_HEAD_SHA="${WORKFLOW_HEAD_SHA:-}"
WORKFLOW_CONCLUSION="${WORKFLOW_CONCLUSION:-}"
DRY_RUN="${MERGE_WHEN_GREEN_DRY_RUN:-}"

# TestSuite jobs that must be green for non-docs PRs.
TESTSUITE_PREFIXES=(build lint-doc lint-fmt lint-opam local-pds)

log() { printf '%s\n' "$*"; }

is_dependabot_login() {
  case "${1:-}" in
    dependabot | dependabot\[bot\] | app/dependabot) return 0 ;;
    *) return 1 ;;
  esac
}

# Docs-only under .github/ is markdown / issue templates / PR template /
# CODEOWNERS / SECURITY / dependabot.yml only. scripts/**, workflow YAML,
# and other shell/config are code (full TestSuite).
is_docs_only_file() {
  local f="${1:-}"
  [[ -z "$f" ]] && return 1
  case "$f" in
    CHANGELOG | CHANGELOG.md | README | README.md) return 0 ;;
    doc | doc/*) return 0 ;;
    *.md) return 0 ;;
    .github/ISSUE_TEMPLATE | .github/ISSUE_TEMPLATE/*) return 0 ;;
    .github/PULL_REQUEST_TEMPLATE.md) return 0 ;;
    .github/CODEOWNERS) return 0 ;;
    .github/SECURITY.md) return 0 ;;
    .github/dependabot.yml) return 0 ;;
    *) return 1 ;;
  esac
}

is_docs_only_file_list() {
  local any=0
  local f
  while IFS= read -r f; do
    [[ -z "$f" ]] && continue
    any=1
    if ! is_docs_only_file "$f"; then
      return 1
    fi
  done
  [[ "$any" -eq 1 ]]
}

# Reads gh `pr checks --json name,state,bucket,workflow` from stdin.
# $1 = docs|full|testsuite
# docs: lint-fmt + lint-doc pass; any fail blocks; other pending is ok.
# full / testsuite: every TestSuite job prefix has a pass; those jobs
# are not pending/fail. Other workflows (lexicon-pin, this job) ignored
# so we do not deadlock waiting for a check we do not wake on.
checks_are_green() {
  local mode="$1"
  jq -e --arg mode "$mode" --arg self "$SELF_WORKFLOW_NAME" \
    --argjson prefixes "$(printf '%s\n' "${TESTSUITE_PREFIXES[@]}" | jq -R . | jq -s .)" '
    def ignored:
      (.workflow // "") == $self or (.name // "") == $self;
    def is_testsuite:
      .name as $n | any($prefixes[]; $n == . or ($n | startswith(. + " ")));
    def is_lint_min:
      .name as $n | ($n | startswith("lint-fmt")) or ($n | startswith("lint-doc"));
    def is_fail:
      (.bucket == "fail")
      or ((.state // "") | test("FAILURE|CANCELLED|TIMED_OUT|STARTUP_FAILURE|ACTION_REQUIRED"));
    def is_pending:
      .bucket == "pending";
    def is_pass:
      .bucket == "pass";
    map(select(ignored | not)) as $c |
    if ($c | length) == 0 then
      false
    elif ($c | map(select(is_fail)) | length) > 0 then
      false
    elif $mode == "docs" then
      ($c | any(is_lint_min and is_pass and (.name | startswith("lint-fmt"))))
      and ($c | any(is_lint_min and is_pass and (.name | startswith("lint-doc"))))
    else
      ($c | map(select(is_testsuite and is_pending)) | length) == 0
      and all($prefixes[]; . as $p |
        $c | any(
          (.name == $p or (.name | startswith($p + " "))) and is_pass
        )
      )
    end
  ' >/dev/null
}

eligible_source() {
  local is_cross="$1"
  local login="$2"
  if [[ "$is_cross" == "false" ]]; then
    return 0
  fi
  is_dependabot_login "$login"
}

pr_files() {
  local n="$1"
  gh pr view "$n" --repo "$REPO" --json files --jq '.files[].path'
}

pr_checks_json() {
  local n="$1"
  # `gh pr checks` exits non-zero when anything is pending/failing.
  gh pr checks "$n" --repo "$REPO" --json name,state,bucket,workflow 2>/dev/null || true
}

should_merge_pr() {
  local n="$1"
  local head_oid="$2"
  local files checks mode

  files="$(pr_files "$n")"
  if [[ -z "$files" ]]; then
    log "PR #$n: no files listed; skip"
    return 1
  fi

  mode="full"
  if printf '%s\n' "$files" | is_docs_only_file_list; then
    mode="docs"
    log "PR #$n: docs-only heuristic (lint-fmt + lint-doc)"
  else
    log "PR #$n: require TestSuite jobs (build, local-pds, lint-*)"
  fi

  # Successful TestSuite workflow_run for this exact SHA is enough for
  # a non-docs PR (and also fine for docs-only).
  if [[ -n "$WORKFLOW_HEAD_SHA" && "$WORKFLOW_HEAD_SHA" == "$head_oid" && "$WORKFLOW_CONCLUSION" == "success" && "$EVENT_NAME" == "workflow_run" ]]; then
    log "PR #$n: TestSuite succeeded for ${head_oid:0:7}"
    return 0
  fi

  checks="$(pr_checks_json "$n")"
  if [[ -z "$checks" || "$checks" == "[]" ]]; then
    log "PR #$n: no checks yet"
    return 1
  fi
  if printf '%s\n' "$checks" | checks_are_green "$mode"; then
    return 0
  fi
  log "PR #$n: checks not green yet ($mode)"
  return 1
}

enable_or_merge() {
  local n="$1"
  log "PR #$n: squash-merging and deleting the head branch"
  if [[ -n "$DRY_RUN" ]]; then
    log "PR #$n: dry-run; not merging"
    return 0
  fi
  gh pr merge "$n" --repo "$REPO" --squash --delete-branch --yes
}

update_behind() {
  local n="$1"
  local oid="$2"
  log "PR #$n: behind main; updating branch"
  if [[ -n "$DRY_RUN" ]]; then
    log "PR #$n: dry-run; not updating branch"
    return 0
  fi
  gh api --method PUT "repos/${REPO}/pulls/${n}/update-branch" \
    -H "Accept: application/vnd.github+json" \
    -f expected_head_sha="$oid" >/dev/null
}

process_prs() {
  local prs
  prs="$(gh pr list --repo "$REPO" --base main --state open --limit 100 \
    --json number,title,isDraft,isCrossRepository,author,mergeable,mergeStateStatus,headRefOid,url)"

  if [[ -z "$prs" || "$prs" == "[]" ]]; then
    log "No open PRs targeting main"
    return 0
  fi

  local n title is_draft is_cross login mergeable status oid
  while IFS=$'\t' read -r n title is_draft is_cross login mergeable status oid; do
    [[ -z "$n" ]] && continue
    log "Considering PR #$n ($title) draft=$is_draft cross=$is_cross author=$login mergeable=$mergeable status=$status"

    if [[ "$is_draft" == "true" ]]; then
      log "PR #$n: draft; skip"
      continue
    fi
    if ! eligible_source "$is_cross" "$login"; then
      log "PR #$n: fork (not Dependabot); skip"
      continue
    fi
    if [[ "$status" == "DRAFT" ]]; then
      log "PR #$n: draft status; skip"
      continue
    fi
    if [[ "$status" == "DIRTY" || "$mergeable" == "false" || "$mergeable" == "CONFLICTING" ]]; then
      log "PR #$n: merge conflict; skip"
      continue
    fi
    if [[ "$status" == "BEHIND" ]]; then
      if update_behind "$n" "$oid"; then
        log "PR #$n: update-branch requested"
      else
        log "PR #$n: update-branch failed (leave for a human)"
      fi
      continue
    fi
    if [[ "$status" == "UNKNOWN" || "$mergeable" == "UNKNOWN" || -z "$mergeable" || "$mergeable" == "null" ]]; then
      log "PR #$n: mergeability not computed yet; skip"
      continue
    fi

    if ! should_merge_pr "$n" "$oid"; then
      continue
    fi

    if [[ "$status" != "CLEAN" && "$status" != "HAS_HOOKS" && "$status" != "UNSTABLE" && "$status" != "BLOCKED" ]]; then
      log "PR #$n: unexpected mergeStateStatus=$status; skip"
      continue
    fi

    # UNSTABLE usually means failing/pending required checks. We already
    # evaluated checks ourselves; still refuse UNSTABLE unless docs-only
    # (pending build/local-pds is expected there).
    if [[ "$status" == "UNSTABLE" ]]; then
      local files
      files="$(pr_files "$n")"
      if ! printf '%s\n' "$files" | is_docs_only_file_list; then
        log "PR #$n: UNSTABLE and not docs-only; skip"
        continue
      fi
    fi

    if enable_or_merge "$n"; then
      log "PR #$n: merged"
    else
      log "PR #$n: merge failed (reviews / branch protection / Allow auto-merge not involved — this workflow merges itself). Leave for a human."
    fi
  done < <(printf '%s\n' "$prs" | jq -r '
    .[] | [
      .number,
      (.title | gsub("\t"; " ")),
      .isDraft,
      .isCrossRepository,
      (.author.login // ""),
      (.mergeable | tostring),
      (.mergeStateStatus // ""),
      (.headRefOid // "")
    ] | @tsv
  ')
  return 0
}

run_self_test() {
  local fail=0
  expect_docs() {
    local path="$1"
    local want="$2"
    if is_docs_only_file "$path"; then
      [[ "$want" == yes ]] || { log "FAIL: $path should not be docs-only"; fail=1; }
    else
      [[ "$want" == no ]] || { log "FAIL: $path should be docs-only"; fail=1; }
    fi
  }

  expect_docs CHANGELOG.md yes
  expect_docs README.md yes
  expect_docs doc/index.mld yes
  expect_docs .github/CONTRIBUTING.md yes
  expect_docs .github/SECURITY.md yes
  expect_docs .github/PULL_REQUEST_TEMPLATE.md yes
  expect_docs .github/ISSUE_TEMPLATE/bug_report.md yes
  expect_docs .github/ISSUE_TEMPLATE/config.yml yes
  expect_docs .github/CODEOWNERS yes
  expect_docs .github/dependabot.yml yes
  expect_docs doc/notes.md yes
  expect_docs leftover.md yes
  expect_docs .github/scripts/foo.sh no
  expect_docs .github/scripts/merge-when-green.sh no
  expect_docs .github/workflows/x.yml no
  expect_docs .github/workflows/test_suite.yml no
  expect_docs .github/workflows/merge-when-green.yml no
  expect_docs .github/FUNDING.yml no
  expect_docs src/lib/atproto.ml no
  expect_docs atproto.opam no
  expect_docs Makefile no

  printf '%s\n' 'CHANGELOG.md' 'README.md' 'doc/index.mld' '.github/CONTRIBUTING.md' | is_docs_only_file_list \
    || { log "FAIL: markdown+doc list should be docs-only"; fail=1; }
  printf '%s\n' 'CHANGELOG.md' 'src/x.ml' | is_docs_only_file_list \
    && { log "FAIL: mixed list should not be docs-only"; fail=1; }
  printf '%s\n' '.github/workflows/x.yml' | is_docs_only_file_list \
    && { log "FAIL: workflow YAML should not be docs-only"; fail=1; }
  printf '%s\n' '.github/scripts/foo.sh' | is_docs_only_file_list \
    && { log "FAIL: .github/scripts must not be docs-only"; fail=1; }
  : | is_docs_only_file_list \
    && { log "FAIL: empty list should not be docs-only"; fail=1; }

  eligible_source false someone || { log "FAIL: same-repo should be eligible"; fail=1; }
  eligible_source true 'dependabot[bot]' || { log "FAIL: Dependabot fork should be eligible"; fail=1; }
  eligible_source true outsider && { log "FAIL: other forks should not be eligible"; fail=1; }

  local docs_pending_build full_green missing_build failing_fmt
  docs_pending_build='[
    {"name":"lint-fmt (ubuntu-22.04, 4.14.1)","state":"SUCCESS","bucket":"pass","workflow":"TestSuite"},
    {"name":"lint-doc (ubuntu-22.04, 4.14.1)","state":"SUCCESS","bucket":"pass","workflow":"TestSuite"},
    {"name":"build (ubuntu-22.04, 4.14.1)","state":"PENDING","bucket":"pending","workflow":"TestSuite"},
    {"name":"Merge when green","state":"SUCCESS","bucket":"pass","workflow":"Merge when green"}
  ]'
  full_green='[
    {"name":"lint-fmt (ubuntu-22.04, 4.14.1)","state":"SUCCESS","bucket":"pass","workflow":"TestSuite"},
    {"name":"lint-doc (ubuntu-22.04, 4.14.1)","state":"SUCCESS","bucket":"pass","workflow":"TestSuite"},
    {"name":"lint-opam (ubuntu-22.04, 4.14.1)","state":"SUCCESS","bucket":"pass","workflow":"TestSuite"},
    {"name":"build (ubuntu-22.04, 4.14.1)","state":"SUCCESS","bucket":"pass","workflow":"TestSuite"},
    {"name":"local-pds","state":"SUCCESS","bucket":"pass","workflow":"TestSuite"},
    {"name":"deploy-pages","state":"SKIPPED","bucket":"skipping","workflow":"TestSuite"},
    {"name":"lexicon-pin","state":"PENDING","bucket":"pending","workflow":"Lexicon pin drift"}
  ]'
  missing_build='[
    {"name":"lint-fmt (ubuntu-22.04, 4.14.1)","state":"SUCCESS","bucket":"pass","workflow":"TestSuite"},
    {"name":"lint-doc (ubuntu-22.04, 4.14.1)","state":"SUCCESS","bucket":"pass","workflow":"TestSuite"},
    {"name":"lint-opam (ubuntu-22.04, 4.14.1)","state":"SUCCESS","bucket":"pass","workflow":"TestSuite"},
    {"name":"local-pds","state":"SUCCESS","bucket":"pass","workflow":"TestSuite"}
  ]'
  failing_fmt='[
    {"name":"lint-fmt (ubuntu-22.04, 4.14.1)","state":"FAILURE","bucket":"fail","workflow":"TestSuite"},
    {"name":"lint-doc (ubuntu-22.04, 4.14.1)","state":"SUCCESS","bucket":"pass","workflow":"TestSuite"}
  ]'

  printf '%s\n' "$docs_pending_build" | checks_are_green docs \
    || { log "FAIL: docs-only should pass with pending build"; fail=1; }
  printf '%s\n' "$docs_pending_build" | checks_are_green full \
    && { log "FAIL: full mode should wait for build/local-pds"; fail=1; }
  printf '%s\n' "$full_green" | checks_are_green full \
    || { log "FAIL: full TestSuite green should pass (lexicon-pin pending ok)"; fail=1; }
  printf '%s\n' "$missing_build" | checks_are_green full \
    && { log "FAIL: missing build should not pass full mode"; fail=1; }
  printf '%s\n' "$failing_fmt" | checks_are_green docs \
    && { log "FAIL: failing lint-fmt should block docs mode"; fail=1; }
  printf '%s\n' '[]' | checks_are_green docs \
    && { log "FAIL: empty checks should not pass"; fail=1; }

  if [[ "$fail" -ne 0 ]]; then
    log "self-test failed"
    return 1
  fi
  log "self-test passed"
}

if [[ "${1:-}" == --self-test ]]; then
  run_self_test
  exit $?
fi

if [[ "${1:-}" == --classify-files ]]; then
  if is_docs_only_file_list; then
    log "docs-only"
    exit 0
  fi
  log "code"
  exit 1
fi

if [[ -z "$REPO" ]]; then
  log "GITHUB_REPOSITORY is required"
  exit 1
fi

process_prs
