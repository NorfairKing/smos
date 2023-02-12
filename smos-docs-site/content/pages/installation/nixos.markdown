---
title: Installation on NixOS
description: Documentation about using Smos from NixOS
---

To install smos on NixOS, [a `home-manager` module](https://rycee.gitlab.io/home-manager/) has been provided as part of the smos repository's flake.

You can use it like this in your `home.nix`:

``` nix
{ pkgs, lib, ... }:
with lib;
let
  smosModule = smos.homeManagerModules.${system}.default
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

See also the [reference documentation for this home manager module](/nix/home-manager-module).

### Optional: Using the cachix cache

If you use cachix, you can configure the `smos.cachix.org` cache as a public cache:

``` nix
nix.binaryCaches = [
  "https://smos.cachix.org"
];
nix.binaryCachePublicKeys = [
  "smos.cachix.org-1:YOs/tLEliRoyhx7PnNw36cw2Zvbw5R0ASZaUlpUv+yM="
];
```

### Server components

To install the server-side components of smos on NixOS, a NixOS module has been provided as part of the smos repository's flake.

``` nix
{ pkgs, lib, ... }:
with lib;
let
  smosModule = smos.nixosModules.${system}.default
in
{
  imports = [
    smosModule
    # [...]
  ];
  services.smos.production = {
    enable = true;
  };
}
```
