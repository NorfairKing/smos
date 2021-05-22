#! /usr/bin/env bash

set -e
set -x

export PATH="$PATH:$(stack path --local-install-root)/bin"
export DEVELOPMENT=True

export SMOS_SERVER_LOG_LEVEL=Debug
export SMOS_SERVER_PORT=8001
export SMOS_SERVER_ADMIN=admin
export SMOS_SERVER_FILE_MIGRATOR_PHASE=0
export SMOS_SERVER_AUTO_BACKUP_PERIOD=3600
export SMOS_SERVER_BACKUP_GARBAGE_COLLECTOR_PERIOD=3600
export SMOS_SERVER_BACKUP_GARBAGE_COLLECTOR_PHASE=60
export SMOS_SERVER_MAX_BACKUPS_PER_USER=5
export SMOS_SERVER_MAX_BACKUP_SIZE_PER_USER=10000000 # 10 MiB
export SMOS_WEB_SERVER_API_URL="http://localhost:$SMOS_SERVER_PORT"
export SMOS_WEB_SERVER_PORT=8000
export SMOS_WEB_SERVER_WEB_URL="http://localhost:$SMOS_WEB_SERVER_PORT"
export SMOS_WEB_SERVER_DATA_DIR=data
export SMOS_WEB_SERVER_DOCS_URL=https://docs.smos.online
export SMOS_WEB_SERVER_LOG_LEVEL=Debug

export LANG=en_GB.UTF-8
export LANGUAGE=en_GB

exec nice -n19 stack build :smos-server :smos-web-server \
  --file-watch --watch-all \
  --exec='./scripts/restart-web-server.sh' \
  --ghc-options='-freverse-errors -O0' \
  --no-nix-pure \
  --fast \
  $@
