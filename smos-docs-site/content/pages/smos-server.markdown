---
title: The Smos Server
description: Documentation for the Smos Server, for hosting your own Smos API
---

You can set up your own Smos server to host your own syncing.

A community sync-server has been set up at [https://api.smos.cs-syd.eu](https://api.smos.cs-syd.eu) in case do not want to do that

### Installation

```
stack install autoexporter
stack install smos-server
```

### Running

To run the server, run the following command:

``` shell
$ smos-server serve
```

