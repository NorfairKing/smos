---
title: Synchronisation
description: Documentation for the setting up synchronisation of your Smos workflow across devices
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

