#!/usr/bin/env bash
# Start Bluesky's official OSS local network (@atproto/dev-env TestNetwork):
# PLC, PDS, AppView, Ozone, bsync. Requires Docker (Postgres+Redis) and Node >= 22.
# Chat (chat.bsky.*) is NOT included — see README.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DEV_DIR="${ROOT}/docker/dev-env"
COMPOSE_FILE="${DEV_DIR}/compose.yaml"
COMPOSE=(docker compose -f "${COMPOSE_FILE}")
PID_FILE="${DEV_DIR}/dev-env.pid"
LOG_FILE="${DEV_DIR}/dev-env.log"
ENV_FILE="${DEV_DIR}/generated.env"
DEV_ENV_VERSION="${DEV_ENV_VERSION:-0.6.4}"

PDS_HOST="${ATP_HOST:-localhost:2583}"
APPVIEW_HOST="${ATP_APPVIEW_HOST:-localhost:2584}"
OZONE_HOST="${ATP_OZONE_HOST:-localhost:2587}"
PLC_ORIGIN="${PLC_ORIGIN:-http://localhost:2582}"
PDS_SCHEME="${ATP_SCHEME:-http}"
PDS_ORIGIN="${PDS_SCHEME}://${PDS_HOST}"
APPVIEW_ORIGIN="${PDS_SCHEME}://${APPVIEW_HOST}"
OZONE_ORIGIN="${PDS_SCHEME}://${OZONE_HOST}"

# Official generateMockSetup accounts (packages/dev-env/src/mock/index.ts).
ACCOUNT_HANDLE="${ATP_LOCAL_HANDLE:-alice.test}"
ACCOUNT_PASSWORD="${ATP_LOCAL_PASSWORD:-hunter2}"
ACCOUNT_EMAIL="${ATP_LOCAL_EMAIL:-alice@test.com}"
BOB_HANDLE="${ATP_LOCAL_HANDLE_2:-bob.test}"
BOB_PASSWORD="${ATP_LOCAL_PASSWORD_2:-hunter2}"
ADMIN_HANDLE="${ATP_OZONE_HANDLE:-admin-mod.test}"
ADMIN_PASSWORD="${ATP_OZONE_PASSWORD:-admin-mod-pass}"

export DB_POSTGRES_URL="${DB_POSTGRES_URL:-postgresql://pg:password@127.0.0.1:5433/postgres}"
export REDIS_HOST="${REDIS_HOST:-127.0.0.1:6380}"

log() { printf '%s\n' "$*"; }
die() { printf 'local-atproto: %s\n' "$*" >&2; exit 1; }

docker_available() {
  command -v docker >/dev/null 2>&1 && docker info >/dev/null 2>&1
}

node_available() {
  command -v node >/dev/null 2>&1 || return 1
  local major
  major="$(node -p "process.versions.node.split('.')[0]")"
  [[ "${major}" -ge 22 ]]
}

# Connection success is enough. PLC serves 404 on /, Ozone 401 without JWT.
wait_http() {
  local url="$1"
  local label="$2"
  local attempts="${3:-90}"
  local i code
  for i in $(seq 1 "${attempts}"); do
    code="$(curl -sS -o /dev/null -w '%{http_code}' --max-time 3 "${url}" 2>/dev/null || true)"
    if [[ "${code}" =~ ^[0-9]{3}$ ]]; then
      log "${label} ready (${url} HTTP ${code})"
      return 0
    fi
    sleep 2
  done
  return 1
}

write_env() {
  local ozone_did="${1:-}"
  cat > "${ENV_FILE}" <<EOF
ATP_SCHEME=${PDS_SCHEME}
ATP_HOST=${PDS_HOST}
ATP_APPVIEW_HOST=${APPVIEW_HOST}
ATP_OZONE_HOST=${OZONE_HOST}
ATP_OZONE_DID=${ozone_did}
PLC_ORIGIN=${PLC_ORIGIN}
ATP_AUTH=${ACCOUNT_HANDLE}:${ACCOUNT_PASSWORD}
ATP_AUTH_BOB=${BOB_HANDLE}:${BOB_PASSWORD}
ATP_AUTH_OZONE=${ADMIN_HANDLE}:${ADMIN_PASSWORD}
ATP_LOCAL_PDS=1
EOF
}

parse_ozone_did() {
  local did=""
  if [[ -f "${LOG_FILE}" ]]; then
    did="$(grep -E 'Ozone service DID' "${LOG_FILE}" | tail -n1 | grep -oE 'did:[a-z0-9]+:[a-zA-Z0-9._:-]+' | tail -n1 || true)"
  fi
  printf '%s' "${did}"
}

cmd_up() {
  if ! docker_available; then
    die "Docker is required for Postgres + Redis (official @atproto/dev-env)"
  fi
  if ! node_available; then
    die "Node.js >= 22 is required to run @atproto/dev-env@${DEV_ENV_VERSION}"
  fi
  "${COMPOSE[@]}" up -d --wait db_test redis_test
  mkdir -p "${DEV_DIR}"
  if [[ ! -d "${DEV_DIR}/node_modules/@atproto/dev-env" ]]; then
    (cd "${DEV_DIR}" && npm install --no-fund --no-audit "@atproto/dev-env@${DEV_ENV_VERSION}")
  fi
  if [[ -f "${PID_FILE}" ]] && kill -0 "$(cat "${PID_FILE}")" 2>/dev/null; then
    log "dev-env already running (pid $(cat "${PID_FILE}"))"
  else
    : > "${LOG_FILE}"
    (
      cd "${DEV_DIR}"
      export DB_POSTGRES_URL REDIS_HOST
      nohup node --enable-source-maps \
        ./node_modules/@atproto/dev-env/dist/bin.js \
        >> "${LOG_FILE}" 2>&1 &
      echo $! > "${PID_FILE}"
    )
    log "started @atproto/dev-env@${DEV_ENV_VERSION} (pid $(cat "${PID_FILE}"))"
  fi
  cmd_wait
}

cmd_down() {
  if [[ -f "${PID_FILE}" ]]; then
    local pid
    pid="$(cat "${PID_FILE}")"
    if kill -0 "${pid}" 2>/dev/null; then
      kill "${pid}" 2>/dev/null || true
      local i
      for i in $(seq 1 20); do
        kill -0 "${pid}" 2>/dev/null || break
        sleep 1
      done
      kill -9 "${pid}" 2>/dev/null || true
    fi
    rm -f "${PID_FILE}"
  fi
  if docker_available; then
    "${COMPOSE[@]}" down -v || true
  fi
}

cmd_wait() {
  local ready=0
  local i
  for i in $(seq 1 180); do
    if [[ -f "${LOG_FILE}" ]] && grep -q 'Dev environment is ready' "${LOG_FILE}"; then
      ready=1
      break
    fi
    if ! kill -0 "$(cat "${PID_FILE}" 2>/dev/null || echo 0)" 2>/dev/null; then
      tail -n 80 "${LOG_FILE}" || true
      die "dev-env process exited before becoming ready"
    fi
    sleep 2
  done
  if [[ "${ready}" -ne 1 ]]; then
    tail -n 80 "${LOG_FILE}" || true
    die "dev-env did not print ready after waiting"
  fi
  wait_http "${PLC_ORIGIN}/" "PLC" 30 || die "PLC not reachable at ${PLC_ORIGIN}"
  if ! wait_http "${PDS_ORIGIN}/xrpc/_health" "PDS" 30; then
    wait_http "${PDS_ORIGIN}/xrpc/com.atproto.server.describeServer" "PDS describeServer" 15 \
      || die "PDS not reachable at ${PDS_ORIGIN}"
  fi
  wait_http "${APPVIEW_ORIGIN}/xrpc/_health" "AppView" 30 \
    || wait_http "${APPVIEW_ORIGIN}/xrpc/app.bsky.actor.getProfile?actor=${ACCOUNT_HANDLE}" "AppView getProfile" 30 \
    || die "AppView not reachable at ${APPVIEW_ORIGIN}"
  wait_http "${OZONE_ORIGIN}/xrpc/tools.ozone.server.getConfig" "Ozone" 15 \
    || die "Ozone not reachable at ${OZONE_ORIGIN}"
  local ozone_did
  ozone_did="$(parse_ozone_did)"
  write_env "${ozone_did}"
  cmd_wait_indexed
}

cmd_wait_indexed() {
  local i
  local url="${APPVIEW_ORIGIN}/xrpc/app.bsky.actor.getProfile?actor=${ACCOUNT_HANDLE}"
  for i in $(seq 1 60); do
    if curl -fsS --max-time 5 "${url}" | grep -q '"did"'; then
      log "AppView indexed ${ACCOUNT_HANDLE}"
      if curl -fsS --max-time 5 \
        "${APPVIEW_ORIGIN}/xrpc/app.bsky.actor.getProfile?actor=${BOB_HANDLE}" \
        | grep -q '"did"'; then
        log "AppView indexed ${BOB_HANDLE}"
      fi
      return 0
    fi
    sleep 2
  done
  die "AppView did not index ${ACCOUNT_HANDLE}"
}

cmd_account() {
  cmd_wait
  log "using official mock accounts ${ACCOUNT_HANDLE} / ${BOB_HANDLE}"
  # Exercise createAccount from the job as well (unique handle).
  local extra="ci$(date +%s).test"
  local tmp code body
  tmp="$(mktemp)"
  code="$(
    curl -sS -o "${tmp}" -w '%{http_code}' --max-time 30 \
      -X POST "${PDS_ORIGIN}/xrpc/com.atproto.server.createAccount" \
      -H 'content-type: application/json' \
      -d "{\"handle\":\"${extra}\",\"email\":\"${extra}@test.local\",\"password\":\"local-pds-ci-password\"}"
  )" || true
  body="$(cat "${tmp}")"
  rm -f "${tmp}"
  if [[ "${code}" == "200" ]]; then
    log "created extra account ${extra}"
  else
    log "extra createAccount HTTP ${code}: ${body}"
  fi
  cat "${ENV_FILE}"
}

cmd_env() {
  if [[ -f "${ENV_FILE}" ]]; then
    sed 's/^/export /' "${ENV_FILE}"
  else
    write_env "$(parse_ozone_did)"
    sed 's/^/export /' "${ENV_FILE}"
  fi
  printf 'export ATP_REQUIRE_LOCAL_PDS=1\n'
}

cmd_logs() {
  if [[ -f "${LOG_FILE}" ]]; then
    cat "${LOG_FILE}"
  else
    log "no ${LOG_FILE}"
  fi
  if docker_available; then
    "${COMPOSE[@]}" logs "$@" || true
  fi
}

usage() {
  cat <<'EOF'
Usage: scripts/local-atproto.sh <up|down|wait|account|env|logs>

  up       Start Postgres+Redis (compose) and official @atproto/dev-env
  down     Stop the Node network and compose volumes
  wait     Block until PDS/AppView/Ozone/PLC are ready and AppView has indexed
  account  Confirm mock alice.test / bob.test and create one extra account
  env      Print export lines for ATP_HOST / ATP_APPVIEW_HOST / ATP_AUTH / ozone
  logs     Show dev-env + compose logs

Pinned package: @atproto/dev-env@0.6.4 (Node >= 22).
Chat is not part of TestNetwork (ozone.chatUrl is http://localhost:2590,
"must run separate chat service"). No official OSS chat backend is started.
EOF
}

case "${1:-}" in
  up) cmd_up ;;
  down) cmd_down ;;
  wait) cmd_wait ;;
  account) cmd_account ;;
  env) cmd_env ;;
  logs) shift; cmd_logs "$@" ;;
  *) usage; exit 2 ;;
esac
