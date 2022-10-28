---
title: Setting up your own Smos server on NixOS
description: Documentation about setting up your own Smos API server on NixOS
---

First [install the NixOS module for the server components of smos](/installation/nixos).
Then activate the api server as follows:


``` nix
{
  services.smos.production = {
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
}
```

See also the [reference documentation for this nixos module](/nix/nixos-module).
