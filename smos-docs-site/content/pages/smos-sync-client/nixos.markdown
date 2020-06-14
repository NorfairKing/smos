---
title: Running on NixOs
---

There is a nix-home-manager module available in the repo as described [in the relevant installation page](/installation/nixos).

Once you have installed it, you can set up automated syncing as follows:

``` nix
programs.smos = {
  enable = true;
  sync = {
    enable = true;
    username = "YOURUSERNAMEHERE";
    password = "YOURPASSWORDHERE";
  };
};
```

A systemd user service will take care of the syncing for you.
To protect your files against any potential bugs in the synchronisation, a backup service has also been provided.
You can enable it as follows:

``` nix
programs.smos = {
  enable = true;
  backup.enable = true;
};
```
