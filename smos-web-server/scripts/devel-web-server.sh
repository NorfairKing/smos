#!/usr/bin/env bash

set -e
set -x

export DEVELOPMENT=True

export SMOS_SERVER_PORT=8001
export SMOS_WEB_SERVER_PORT=8000
export SMOS_SERVER_LOG_LEVEL=Debug
export SMOS_WEB_SERVER_LOG_LEVEL=Debug


nice -n19 stack install :smos-server :smos-web-server \
  --file-watch \
  --exec='./scripts/restart-web-server.sh' \
  --ghc-options='-freverse-errors -O0' \
  --no-nix-pure \
  --fast \
  $@
