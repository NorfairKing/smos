---
title: Building/Installation
date: 2018-11-13
---
## Default Installation

Clone the repository:

```
$ git clone https://github.com/NorfairKing/smos
```

### Building with Stack

Use [Stack](https://haskellstack.org) to install Smos with the default configuration:

```
$ stack install :smos
```

#### Troubleshooting on Arch linux

*Do not install `stack` or any other haskell packages using pacman.*
This is unlikely to build at all and will most likely cause in bugs if it does.
Instead, install `stack` by following the documentation [on this page](https://docs.haskellstack.org/en/stable/README/#how-to-install)

### Building with Nix

If you are building with nix, you probably know what you are doing.
Start with a regular build, and go from there using the multiple results:

```
$ nix-build
```

In particular, you will want `smos`, `smos-query`,`smos-archive` and possibly `smos-convert-org`.

#### Troubleshooting

When you first run a `smos` executable that has been built with Nix, you may
get an error like the following:

```
smos: setupTerm: Couldn't look up terminfo entry "rxvt-unicode-256color"
```

In this case, you can probably solve the problem by setting the `TERM` variable
to `xterm`:

```
$ TERM=xterm ./result/bin/smos
```

## Custom installation

Should you wish to customise your `smos` installation, or customise the `smos`
build entirely, then follow the appropriate guides
[here](/customisation-default.html) or
[here](/customisation-haskell.html), respectively.
