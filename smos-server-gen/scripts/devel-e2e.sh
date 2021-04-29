#!/usr/bin/env bash

stack install smos-server-gen --fast --file-watch --exec='./scripts/restart-e2e.sh'
