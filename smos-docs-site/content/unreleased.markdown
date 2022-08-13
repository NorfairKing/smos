### Added

* `smos-scheduler`: Support for haircut recurrence

### Changed

* Fixed that the automatic local backup service would fail if the workflow directory does not exist.
* `smos-scheduler` overhaul:
  * The `smos-scheduler` tool is now stateless.
    It now uses the workflow directory to compute 'last run's instead of a state file.
  * The `next` command is now colourful.
  * A newly added schedule is now immediately run, instead of waiting until the second run.
  * The minimal scheduling interval has been removed.
    This means that schedules that are run "every minute" are no longer supported.
  * Rewrote the documentation
