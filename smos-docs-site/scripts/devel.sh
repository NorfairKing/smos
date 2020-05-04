#!/usr/bin/env bash

set -e
set -x

nice -n19 stack install :smos-docs-site \
  --file-watch \
  --exec='./scripts/restart.sh' \
  --ghc-options="-freverse-errors -DDEVELOPMENT -O0" \
  --fast \
  $@

