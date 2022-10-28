---
title: Setting up your own Smos Web Server on Nixos
description: Documentation for the Smos Web Server, for setting up your own online Smos usage on NixOS
---

First [install the NixOS module for the server components of smos](/installation/nixos).
Then activate the api server as follows:

``` nix
{
  services.smos.production {
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
}

```

See also the [reference documentation for this nixos module](/nix/nixos-module).

