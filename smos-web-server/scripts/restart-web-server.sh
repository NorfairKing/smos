#!/usr/bin/env bash
set -e
set -x

killall 'smos-web-server serve' || true
export SMOS_WEB_SERVER_DATA_DIR=/tmp/smos-web-server/data
smos-web-server serve &
