#!/usr/bin/env bash

set -e -x

export DEVELOPMENT=True
export SMOS_SERVER_PORT=8000
export SMOS_SERVER_URL=localhost:${SMOS_SERVER_PORT}

stack install smos-server-gen \
  --fast \
  --file-watch \
  --ghc-options='-freverse-errors -O0' \
  --exec='./scripts/restart-e2e.sh' \
  --no-nix-pure
