#!/usr/bin/env bash

set -e
set -x

export DEVELOPMENT=True

export SMOS_SERVER_LOG_LEVEL=Debug
export SMOS_SERVER_PORT=8001
export SMOS_SERVER_ADMIN=admin
export SMOS_WEB_SERVER_API_URL="http://localhost:$SMOS_SERVER_PORT"
export SMOS_WEB_SERVER_DATA_DIR=data
export SMOS_WEB_SERVER_DOCS_URL=https://docs.smos.online
export SMOS_WEB_SERVER_LOG_LEVEL=Debug
export SMOS_WEB_SERVER_PORT=8000

export LANG=en_GB.UTF-8
export LANGUAGE=en_GB

nice -n19 stack install :smos-server :smos-web-server \
  --file-watch --watch-all\
  --exec='./scripts/restart-web-server.sh' \
  --ghc-options='-freverse-errors -O0' \
  --no-nix-pure \
  --fast \
  $@
