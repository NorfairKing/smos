---
title: Setting up your own Smos server on NixOS
description: Documentation about setting up your own Smos API server on NixOS
---

A NixOs module has to run your own smos server on NixOs has been provided at `nix/nixos-module.nix` and can be used as follows:

``` nix
{ lib, pkgs, config, ... }:
let
  smos-production = (
    import (
      builtins.fetchGit {
        url = "https://github.com/NorfairKing/smos";
        rev = "0000000000000000000000000000000000000000"; # Put a recent commit hash here.
        ref = "master";
      } + "/nix/nixos-module.nix"
    ) { envname = "production"; }
  );
in
{
  imports = [
    smos-production
  ];
  config = {
    services.smos = {
      production = {
        enable = true;
        api-server = {
          enable = true;
          log-level = "Warn";
          hosts = [ "api.smos.example.com" ];
          port = 8402;
          local-backup = {
            enable = true;
          };
        };
      };
    };
  };
}
```

See also the [reference documentation for this nixos module](/nix/nixos-module).
