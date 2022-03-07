---
title: API Server
description: How to contribute to the Smos API Server
---

## Setting up a feedback loop

```
$ stack test smos-server-gen --file-watch
```

## End-to-end tests

To add to the end-to-end tests, use this feedback loop:
```
$ nix-shell
nix-shell $ feedback server-e2e
```

