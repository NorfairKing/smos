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
$ cd smos-server-gen
$ ./scripts/devel-e2e.sh
```

