---
title: API Server
description: How to contribute to the Smos API Server
---

## Setting up a feedback loop

To run a feedback loop for the server tests, you can use stack:

```
$ stack test smos-server-gen --file-watch
```

To contribute to the end-to-end tests, use this feedback loop:

```
$ nix develop
nix $ feedback server-e2e
```

