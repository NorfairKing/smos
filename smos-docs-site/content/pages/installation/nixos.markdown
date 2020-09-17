---
title: Installation on Nixos
---

To install smos on nixos, [a `home-manager` module](https://rycee.gitlab.io/home-manager/) has been provided in the smos repository at `nix/program.nix`.

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
    } + "/nix/program.nix";
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
