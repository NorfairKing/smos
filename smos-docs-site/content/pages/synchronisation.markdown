---
title: Synchronisation
---

## Synchronising Client

Your can synchronise your workflow directory using `smos-sync-client`.
This synchronisation works with arbitrary amounts of time between synchronisations,
so it is perfect for taking your laptop onto an airplane.

A community sync-server has been set up at https://api.smos.cs-syd.eu .

### Configuration

To get started, configure smos to use a sync server.
You'll need to add the following to your smos config file:

``` yaml
sync:
  server-url: "api.smos.cs-syd.eu"
  username: YOUR_USERNAME_HERE
```

Be sure to set the username that you want.

### Registration

Then register your username at the sync server:

``` shell
$ smos-sync-client register
```

You will be prompted for a password.

### Password

If no password is configured, you will be prompted for a password.
This is the most secure, but does not work well if you want to automate synchronisation.

You can also pass in a password using:

- The `--password` option on the command line
- The `SMOS_SYNC_CLIENT_PASSWORD` environment variable
- The `password` field in your smos config file.

### Synchronisation

To synchronise your workflow directory, run the following command:

``` shell
$ smos-sync-client sync
```

Run this command periodically to keep your workflow directory synchronised.


### Setting up sychronisation on nixos

There is a nix-home-manager module available in the repo at `nix/program.nix`.

You can use it as follows:

``` nix
{ pkgs, lib, ... }:
with lib;
let
  smosModule = (builtins.fetchGit {
    url = "https://github.com/NorfairKing/smos";
    ref = "master";
    rev = "0000000000000000000000000000000000000000"; # Add a recent version here.
  } + "/nix/program.nix");
in
{
  imports = [
    smosModule
    # [...]
  ];
  programs.smos = {
    enable = true;
    sync = {
      enable = true;
      username = "YOURUSERNAMEHERE";
      password = "YOURPASSWORDHERE";
    };
  };
}
```

## Running your own synchronisation server

The synchronisation server is open-source, so you can just run it yourself.
See [the installation guide](/building-installation.html) for information on installing it.

To run the server, run the following command:

``` shell
$ smos-server serve
```

Run `smos-server --help` for an overview of the options that you can use to configure the server.

### Running your own synchronisation server on nixos

There is a nixos module available in the repo at `nix/module.nix`.

You can use it as follows:

```
{ pkgs, lib, config, ... }:
let
  smosModule =
    (
      import (
        builtins.fetchGit {
          url = "https://github.com/NorfairKing/smos.git";
          rev = "0000000000000000000000000000000000000000"; # Fill in using recent master
          ref = "development";
        } + "/nix/module.nix"
      ) { envname = "production"; }
    )
in
{
  imports = [
    smosModule;
  ];
  config = {
    services.smos.production = {
      enable = true;
      sync-server = {
        enable = true;
        hosts = [ "api.your.own.website.com" ];
        port = 8000;
      };
    };
  };
}
```
