---
title: Web Server
description: How to contribute to the Smos Web Server
---

## Setting up a feedback loop

To run a feedback loop for the web server tests, you can use stack:

```
$ stack test smos-web-server --file-watch
```

To run the server itself, so you can try things in the browser, you can use `feedback`:

```
$ nix-shell
nix-shell $ feedback web-server
```
