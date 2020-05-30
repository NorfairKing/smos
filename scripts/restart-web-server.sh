#!/usr/bin/env bash
set -x

killall 'smos-web-server serve' || true

set -e

smos-web-server serve &
