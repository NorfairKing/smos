#!/usr/bin/env bash

set -e
set -x

killall smos-docs-site || true
smos-docs-site serve &
