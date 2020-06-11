---
title: Running on NixOs
---

There is a nix-home-manager module available in the repo as described [in the relevant installation page](/page/installation_nixos).

Once you have installed it, you can set up automated calendar syncing as follows:

``` nix
programs.smos = {
  enable = true;
  calendar = {
    enable = true;
    destination = "calendar.smos";
    sources = [
      "https://calendar.google.com/calendar/ical/example%40gmail.com/private-00000000000000000000000000000000/basic.ics"
    ];
  };
};
```
