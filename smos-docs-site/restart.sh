#!/usr/bin/env bash

set -e
set -x

killall smos-docs-site || true
sleep 0.1
smos-docs-site watch &
