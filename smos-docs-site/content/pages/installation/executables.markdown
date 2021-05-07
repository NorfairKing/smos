---
title: Downloading executables
description: Documentation about using smos without any building via the statically-linked executables
---

### Statically linked executables

Each release contains a `release.zip` archive with statically linked executables.
You can download it from [the "Assets" section on github](https://github.com/NorfairKing/smos/releases/latest).

### Building with Nix

If you are building with nix, you probably know what you are doing.
Start with a regular build, and go from there using the multiple results:

```
$ nix-build
```

To install the executables in your user environment directly, you _could_ do this:

```
nix-env -if default.nix
```

However you probably don't want to do this. If you are using NixOS and or nix' home-manager, have a look at the [NixOS installation page](/installation/nixos) instead.

### Optional: Using the cachix cache

If you use cachix, you can use `cachix use smos` to use the smos cachix cache.

### Troubleshooting

When you first run a `smos` executable that has been built with Nix, you may
get an error like the following if you use a particular terminal emulator like `urxvt`:

```
smos: setupTerm: Couldn't look up terminfo entry "rxvt-unicode-256color"
```

In this case, you can probably solve the problem by setting the `TERM` variable
to `xterm`:

```
$ TERM=xterm ./result/bin/smos
```
