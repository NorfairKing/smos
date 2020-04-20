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
$ stack install autoexporter
$ stack install
```

#### Troubleshooting on Arch linux

*Do not install `stack` or any other haskell packages using pacman.*
This is unlikely to build at all and will most likely cause in bugs if it does.
Instead, install `stack` by following the documentation [on this page](https://docs.haskellstack.org/en/stable/README/#how-to-install)

### Building with Nix

If you are building with nix, you probably know what you are doing.
Start with a regular build, and go from there using the multiple results:

```
$ nix-build nix/release.nix
```

To install the executables in your user environment directly:

```
nix-env -if ./nix/release.nix
```

#### Nix home manager module

There is a nix home manager available in the smos repository at `nix/program.nix`, and you can use it as follows:


``` nix
{ pkgs, lib, ... }:
with lib;
let
  smosModule = (builtins.fetchGit {
    url = "https://github.com/NorfairKing/smos";
    ref = "master";
    rev = "0000000000000000000000000000000000000000"; # Add a recent version here.
  } + "/nix/program.nix");
in
{
  imports = [
    smosModule
    # [...]
  ];
  programs.smos = {
    enable = true;
    backup = {
      enable = true;
    };
    sync = {
      enable = true;
      username = "YOURUSERNAMEHERE";
      password = "YOURPASSWORDHERE";
    };
  };
}
```

Note that we have to use `builtins.fetchGit` and cannot use `fetchFromGitHub` because this needs to be fetched at evaluation time.


#### Using the cachix cache

If you use cachix, you can use `cachix use smos` to use the smos cachix cache.

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
