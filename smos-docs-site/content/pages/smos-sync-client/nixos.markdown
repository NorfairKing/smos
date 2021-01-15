---
title: Running on NixOS
description: Documentation about setting up automated synchronisation of your Smos workflow on NixOS
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

Note that [systemd timers do not run while a user is not logged in](https://unix.stackexchange.com/questions/292913/run-users-systemd-timer-while-they-dont-have-any-open-session), so you may have to turn on lingering in order to automatically sync while the user is not logged in.
