---
title: Installation on Nixos
description: Documentation about using Smos from NixOS
---

To install smos on nixos, [a `home-manager` module](https://rycee.gitlab.io/home-manager/) has been provided in the smos repository at `nix/home-manager-module.nix`.

You can use it like this in your `home.nix`:

``` nix
{ pkgs, lib, ... }:
with lib;
let
  smosModule =
    builtins.fetchGit {
      url = "https://github.com/NorfairKing/smos";
      rev = "0000000000000000000000000000000000000000"; # Put a recent commit hash here.
      ref = "master";
    } + "/nix/home-manager-module.nix";
in
{
  imports = [
    smosModule
    # [...]
  ];
  programs.smos = {
    enable = true;
  };
}
```

You can also declare your smos configuration using the `programs.smos.config` attribute.
This will be translated to your configuration file and put into the right place automatically.

### Optional: Using the cachix cache

If you use cachix, you can configure the `smos.cachix.org` cache as a public cache:

``` nix
nix.binaryCaches = [
  https://smos.cachix.org
];
nix.binaryCachePublicKeys = [
  "smos.cachix.org-1:YOs/tLEliRoyhx7PnNw36cw2Zvbw5R0ASZaUlpUv+yM="
];
```
