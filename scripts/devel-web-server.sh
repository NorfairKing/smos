#!/usr/bin/env bash

set -e
set -x

stack install :smos-web-server \
  --file-watch \
  --fast \
  --ghc-options=-freverse-errors \
  --exec='./scripts/restart-web-server.sh' \
  $@
