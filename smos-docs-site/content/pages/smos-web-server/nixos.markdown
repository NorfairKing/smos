---
title: Setting up your own Smos Web Server on Nixos
description: Documentation for the Smos Web Server, for setting up your own online Smos usage on NixOS
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
        web-server = {
          enable = true;
          log-level = "Warn";
          hosts = [ "smos.example.com" ];
          port = 8403;
          docs-url = "https://${builtins.head config.services.smos.production.docs-site.hosts}";
          api-url = "https://${builtins.head config.services.smos.production.api-server.hosts}";
        };
      };
    };
  };
}

```

See also the [reference documentation for this nixos module](/nix/nixos-module).

