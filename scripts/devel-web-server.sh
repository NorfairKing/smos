#!/usr/bin/env bash

set -e
set -x

export DEVELOPMENT=True

export SMOS_WEB_SERVER_LOG_LEVEL=Debug

nice -n19 stack install :smos-web-server \
  --file-watch \
  --exec='./scripts/restart-web-server.sh' \
  --ghc-options='-freverse-errors -O0' \
  --no-nix-pure \
  --fast \
  $@
