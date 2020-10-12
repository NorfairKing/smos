#!/usr/bin/env bash

set -e
set -x

export DEVELOPMENT=True
export PATH="$HOME/.local/bin:$PATH"

nice -n19 stack install :smos-docs-site \
  --file-watch \
  --exec='./scripts/restart.sh' \
  --ghc-options="-freverse-errors -O0" \
  --no-nix-pure \
  --fast \
  $@

