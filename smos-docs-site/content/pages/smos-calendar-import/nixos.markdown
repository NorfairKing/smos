---
title: Running on NixOs
---

There is a nix-home-manager module available in the repo as described [in the relevant installation page](/installation/nixos).

Once you have installed it, you can set up automated calendar syncing as follows:

``` nix
programs.smos = {
  enable = true;
  calendar = {
    enable = true;
    destination = "calendar.smos";
    sources = [
      {
        name = "Personal";
        destination = "calendar.smos";
        source = "https://calendar.google.com/calendar/ical/example%40gmail.com/private-00000000000000000000000000000000/basic.ics";
      }
    ];
  };
};
```
