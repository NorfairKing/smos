#!/usr/bin/env bash
set -e
set -x

killall 'smos-web-server' || true
killall 'smos-server' || true

export SMOS_WEB_SERVER_API_URL="http://localhost:$SMOS_SERVER_PORT"
export SMOS_WEB_SERVER_DATA_DIR=data

smos-server serve &

smos-web-server serve &
