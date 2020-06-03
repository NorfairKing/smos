---
title: The Smos Web Server
---

You can set up your own Smos Web Server to host your own web interface for smos.

### Installation

#### Front-end

The web server front-end is written in Purescript, so you will need to have a few tools set up.
The easiest way to get set up is using `nix-shell` to get `yarn`, `nodejs`, `purs` and `spago`:

```
$ nix-shell
nix-shell $ yarn bundle
```

#### Back-end

```
stack install autoexporter
stack install smos-server
```

### Running

To run the server, run the following command:

``` shell
$ smos-web-server serve
```

