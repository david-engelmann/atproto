#!/usr/bin/env bash
# Back-compat wrapper: the local stack is now official @atproto/dev-env.
exec "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/local-atproto.sh" "$@"
