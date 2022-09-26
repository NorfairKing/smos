### Added 

* The home manager module now does a config check for `smos-query`, `smos-scheduler` and `smos-notify` during activation.
* The home manager module now does an extra local backup during activation.
* Editor integration: You can now use `vim` or `emacs` to edit the contents of an entry.
* Sandbox mode, so the editor integration cannot do damage in the web version.

### Changed

* `smos-scheduler`: Allow the schedule template to be specified as an absolute path as well.
* `smos-query`: Fixed that the agenda entries in smos-query had incorrect
  pretty relative days for timestamps with a day-level granularity.
* `smos-query`: Improved metavars for optparse documentation.
* `smos-web-server`: Change the `TUIR` and `TUIInstanceR` routes to not accept any path.
* `smos-web-server`: Use `playground.smos` in the playground instead of `example.smos`.
* `smos-web-server`: Open the editor in the workflow directory by default, instead of `example.smos`.
