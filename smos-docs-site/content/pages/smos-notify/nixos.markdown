---
title: Running on NixOS
description: Documentation about automatically sending calendar notifications on NixOS
---

There is a nix-home-manager module available in the repo as described [in the relevant installation page](/installation/nixos).

Once you have installed it, you can set up automated notifications as follows:

``` nix
programs.smos = {
  enable = true;
  notify = {
    enable = true;
  };
};
```
