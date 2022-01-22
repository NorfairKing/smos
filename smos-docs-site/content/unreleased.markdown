
### Changed

* Upgraded to `lts-18.16` and the `nixos-21.11` branch of `nixpkgs`.
* Reduced the closure size of the nixos module and the home manager module.
* `smos-server`: Simplified to be single-command, so that you don't have to use `serve` anymore.
* `smos-web-server`: Simplified to be single-command, so that you don't have to use `serve` anymore.
* `smos-notify`: Fixed a bug where `smos-notify` would fail to send a notification when the notification summary or description started with a `-` character.
* `smos-data`: Forward-compatibility with data format version 2.0.0.
* `smos-data`: Clearer data formats with more comprehensive naming.
