### Changed

* `nixos-module`: Changed `openFirewall`s default to `false`.
* `smos-calendar-import`: Retry requests to sources when they get 4XX responses as well, instead of only when the request fails entirely.
* `home-manager-module`: Allow configuring `smos-calendar-import`s frequency.
