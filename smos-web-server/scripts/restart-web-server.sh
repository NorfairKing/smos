#!/usr/bin/env bash
set -e
set -x

killall 'smos-web-server serve' || true
smos-web-server serve &
