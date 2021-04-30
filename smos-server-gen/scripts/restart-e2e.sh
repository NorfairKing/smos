#!/usr/bin/env bash

set -ex

killall smos-server || true
smos-server serve &

sleep 0.5

smos-server-end-to-end-test
