#!/usr/bin/env bash
set -e
set -x

killall 'smos-web-server' || true
killall 'smos-server' || true


smos-server &

sleep 0.5

smos-web-server &
