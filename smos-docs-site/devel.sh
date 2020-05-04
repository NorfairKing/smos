#!/usr/bin/env bash

set -e
set -x

stack install smos-docs-site --file-watch --exec='./restart.sh'
