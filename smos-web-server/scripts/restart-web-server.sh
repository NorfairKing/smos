#!/usr/bin/env bash
set -e
set -x

killall 'smos-web-server' || true
killall 'smos-server' || true

export SMOS_SERVER_PORT=80001
export SMOS_WEB_SERVER_PORT=8000
export SMOS_WEB_SERVER_API_URL="http://localhost:$SMOS_SERVER_PORT"
export SMOS_WEB_SERVER_DATA_DIR=/tmp/smos-web-server/data

smos-server serve &

smos-web-server serve &
