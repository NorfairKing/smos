---
title: Running on NixOs
description: Documentation about setting up automated scheduling of projects on NixOS
---

There is a nix-home-manager module available in the repo as described [in the relevant installation page](/installation/nixos).

Once you have installed it, you can set up scheduler checking and activation as follows:

``` nix
programs.smos = {
  enable = true;
  scheduler = {
    enable = true;
    schedule = [
      {
        description = "Weekly tasks";
        template = "templates/weekly.soms.template";
        destination = "projects/weekly-[ %V | monday ].smos";
        schedule = "0 12 * * 7" # Cron schedule: "At 12:00 on Sunday.";
      }
    ];
  };
};
```

See also the [reference documentation for this home manager module](/nix/home-manager-module).
