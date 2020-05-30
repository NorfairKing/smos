#!/usr/bin/env bash
set -e
set -x

cd smos-web-server
killall 'smos-web-server serve' || true
smos-web-server serve &
