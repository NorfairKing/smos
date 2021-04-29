#!/usr/bin/env bash

set -ex

killall smos-server || true
export SMOS_SERVER_PORT=8000
smos-server serve &

sleep 0.5

export SMOS_SERVER_URL=localhost:${SMOS_SERVER_PORT}
smos-server-end-to-end-test
