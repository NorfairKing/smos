---
title: Building/Installation
---


Smos is currently developed on Linux.
It _should_ "just work" on MacOS but this is not tested.
It _should_ "just work" on Windows' subsystem for linux, but this is not tested.
It probably won't work on Windows as-is. (See [Issue 52](https://github.com/NorfairKing/smos/issues/52))

## Default Installation

Clone the repository:

```
$ git clone https://github.com/NorfairKing/smos
```

### Building with Stack

Use [Stack](https://haskellstack.org) to install Smos with the default configuration:

```
$ stack install autoexporter
$ stack install
```

#### Troubleshooting on Arch linux

*Do not install `stack` or any other haskell packages using pacman.*
This is unlikely to build at all and will most likely cause in bugs if it does.
Instead, install `stack` by following the documentation [on this page](https://docs.haskellstack.org/en/stable/README/#how-to-install)

### Custom installation

Should you wish to customise your `smos` installation, or customise the `smos`
build entirely, then follow the appropriate guides
[here](/customisation-default.html) or
[here](/customisation-haskell.html), respectively.
