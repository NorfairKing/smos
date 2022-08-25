### Added 

* The home manager module now does a config check for `smos-query`, `smos-scheduler` and `smos-notify` during activation.

### Changed

* `smos-scheduler`: Allow the schedule template to be specified as an absolute path as well.
* `smos-query`: Fixed that the agenda entries in smos-query had incorrect
  pretty relative days for timestamps with a day-level granularity.
