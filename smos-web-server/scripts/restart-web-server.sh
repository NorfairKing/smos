#!/usr/bin/env bash
set -e
set -x

killall 'smos-web-server' || true
killall 'smos-server' || true


smos-server serve &

sleep 0.1

smos-web-server serve &
