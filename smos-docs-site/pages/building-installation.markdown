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

Use [Stack](haskellstack.org) to install Smos with the default configuration:

```
$ stack install :smos
```

#### Troubleshooting on Arch linux

*Do not install `stack` or any other haskell packages using pacman.*
This is unlikely to build at all and will most likely cause in bugs if it does.
Instead, install `stack` by following the documentation [on this page](https://docs.haskellstack.org/en/stable/README/#how-to-install)

### Building with Nix

There are a few relevant targets that you may want to use.
Most likely, you will want to use.

```
$ nix-build nix/release.nix
$ ./result/bin/smos
```

Alternatively, you can also use:

```
$ nix-build -A smos-static nix/release.nix
$ ./result/bin/smos
```

The difference is that the latter will not build and run test suites that are external to the package that they are testing.
These are all the test suites in packages that end in `-gen`.

#### Troubleshooting

When you first run a `smos` executable that has been built with Nix, you may get an error like the following:

```
smos: setupTerm: Couldn't look up terminfo entry "rxvt-unicode-256color"
```

In this case, you can probably solve the problem by setting the `TERM` variable to `xterm`:

```
$ TERM=xterm ./result/bin/smos
```

## Custom installation

Should you wish to change the key controls, you can do this by making your own
little Haskell project (also with stack), define the configuration you want to
use and pass it to the [`smos`](https://github.com/NorfairKing/smos/blob/development/smos/src/Smos.hs#L29)
library function.
