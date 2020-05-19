#!/usr/bin/env bash

set -e
set -x

export DEVELOPMENT=True

nice -n19 stack install :smos-docs-site \
  --file-watch \
  --exec='./scripts/restart.sh' \
  --ghc-options="-freverse-errors -O0" \
  --pedantic \
  --no-nix-pure \
  --fast \
  $@

