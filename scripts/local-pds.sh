#!/usr/bin/env bash
# Start / stop the local PDS (+ PLC) stack and create the CI test account.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COMPOSE_FILE="${ROOT}/docker/pds/compose.yaml"
COMPOSE=(docker compose -f "${COMPOSE_FILE}")

PDS_HOST="${ATP_HOST:-localhost:2583}"
PDS_SCHEME="${ATP_SCHEME:-http}"
PDS_ORIGIN="${PDS_SCHEME}://${PDS_HOST}"
PLC_ORIGIN="${PLC_ORIGIN:-http://localhost:2582}"

ACCOUNT_HANDLE="${ATP_LOCAL_HANDLE:-alice.test}"
ACCOUNT_EMAIL="${ATP_LOCAL_EMAIL:-alice@test.local}"
ACCOUNT_PASSWORD="${ATP_LOCAL_PASSWORD:-local-pds-ci-password}"

log() { printf '%s\n' "$*"; }
die() { printf 'local-pds: %s\n' "$*" >&2; exit 1; }

docker_available() {
  command -v docker >/dev/null 2>&1 && docker info >/dev/null 2>&1
}

wait_http() {
  local url="$1"
  local label="$2"
  local attempts="${3:-90}"
  local i
  for i in $(seq 1 "${attempts}"); do
    if curl -fsS --max-time 2 "${url}" >/dev/null 2>&1; then
      log "${label} ready (${url})"
      return 0
    fi
    sleep 2
  done
  return 1
}

cmd_up() {
  if ! docker_available; then
    die "Docker is required to start the local PDS"
  fi
  "${COMPOSE[@]}" up -d --build
  cmd_wait
}

cmd_down() {
  if ! docker_available; then
    die "Docker is required to stop the local PDS"
  fi
  "${COMPOSE[@]}" down -v
}

cmd_wait() {
  if ! wait_http "${PLC_ORIGIN}/" "PLC"; then
    "${COMPOSE[@]}" logs plc plc-db || true
    die "PLC did not become ready at ${PLC_ORIGIN}"
  fi
  if ! wait_http "${PDS_ORIGIN}/xrpc/_health" "PDS"; then
    # Some PDS builds only expose describeServer.
    if ! wait_http "${PDS_ORIGIN}/xrpc/com.atproto.server.describeServer" "PDS describeServer"; then
      "${COMPOSE[@]}" logs pds plc || true
      die "PDS did not become ready at ${PDS_ORIGIN}"
    fi
  fi
}

cmd_account() {
  cmd_wait
  local body
  local tmp
  tmp="$(mktemp)"
  local code
  code="$(
    curl -sS -o "${tmp}" -w '%{http_code}' --max-time 30 \
      -X POST "${PDS_ORIGIN}/xrpc/com.atproto.server.createAccount" \
      -H 'content-type: application/json' \
      -d "{\"handle\":\"${ACCOUNT_HANDLE}\",\"email\":\"${ACCOUNT_EMAIL}\",\"password\":\"${ACCOUNT_PASSWORD}\"}"
  )" || true
  body="$(cat "${tmp}")"
  rm -f "${tmp}"
  if [[ "${code}" == "200" ]]; then
    log "created ${ACCOUNT_HANDLE}"
  elif echo "${body}" | grep -Eqi 'HandleAlreadyExists|AlreadyExists|handle already'; then
    log "account ${ACCOUNT_HANDLE} already exists"
  else
    die "createAccount failed (HTTP ${code}): ${body}"
  fi
  printf 'ATP_SCHEME=%s\n' "${PDS_SCHEME}"
  printf 'ATP_HOST=%s\n' "${PDS_HOST}"
  printf 'ATP_AUTH=%s:%s\n' "${ACCOUNT_HANDLE}" "${ACCOUNT_PASSWORD}"
  printf 'ATP_LOCAL_PDS=1\n'
}

cmd_env() {
  printf 'export ATP_SCHEME=%q\n' "${PDS_SCHEME}"
  printf 'export ATP_HOST=%q\n' "${PDS_HOST}"
  printf 'export ATP_AUTH=%q\n' "${ACCOUNT_HANDLE}:${ACCOUNT_PASSWORD}"
  printf 'export ATP_LOCAL_PDS=1\n'
}

usage() {
  cat <<'EOF'
Usage: scripts/local-pds.sh <up|down|wait|account|env|logs>

  up       Build/start PLC + official PDS and wait until healthy
  down     Stop the stack and remove volumes
  wait     Block until PLC and PDS answer HTTP
  account  Create the local test account (alice.test) via createAccount
  env      Print export lines for ATP_HOST / ATP_AUTH / ATP_SCHEME
  logs     Show compose logs
EOF
}

case "${1:-}" in
  up) cmd_up ;;
  down) cmd_down ;;
  wait) cmd_wait ;;
  account) cmd_account ;;
  env) cmd_env ;;
  logs) "${COMPOSE[@]}" logs "${@:2}" ;;
  *) usage; exit 2 ;;
esac
