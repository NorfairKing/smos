### Changed

* `nixos-module`: Changed `openFirewall`s default to `false`.
* `smos-calendar-import`: Retry requests to sources when they get 4XX responses as well, instead of only when the request fails entirely.
* `home-manager-module`: Allow configuring every timer's frequency.
* `smos-web-server`: Mention bookings on the front page.
* `smos`: Experimental windows support
* `smos`: Split out a `smos-e2e` package so the smos executable can be built on Windows
* `smos-server`: Fixed a bug with bookings (and backups) crashing when done concurrently with another database operation.
