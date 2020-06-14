---
title: Installation with Nix
---

### Building with Nix

If you are building with nix, you probably know what you are doing.
Start with a regular build, and go from there using the multiple results:

```
$ nix-build -A release
```

To install the executables in your user environment directly:

```
nix-env -if default.nix -A release
```

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
