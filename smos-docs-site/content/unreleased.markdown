### Added

* `smos-jobhunt`: A new tool to help with application processes.
* `smos-cli`: A library for making smos CLIs.
* `smos-directory`: A library for all the configuration related to smos directories.

### Changed

* `smos-github`:
  Fixed that the `smos-github` config was not included in the config file
  generated in the home manager module.
* `smos-calendar-import`:
  Refactored most of the `smos-calendar-import` logic into [a separate `ical` library](https://github.com/NorfairKing/ical).
  This means that:
  1. Calendar imports should be more accurate now; in particular around timezone changes.
  2. Smos can now more gracefully handle services outputting invalid `.ics` files.
     Indeed, Google, Apple, and Microsoft all sometimes spit out invalid `.ics` files.
  3. Importing the calendar is sped up by a lot, because that library computes
     recurrence and timezone resolution more efficiently.
* `smos-report`: Sped up timestamp calculations by computing an `Interval` from a `Period` ahead of time.
* `smos-query`:
  Refactored smos-report's handling of periods so that it no longer calculates
  periods at the end or beginning of a year incorrectly.
* `smos-query`:
  Fixed that smos-report would get timezone information wrong about timestamps
  in the same timezone but in summer time (in winter) or vice versa.
* `smos-query`: Support for the following time periods:
  * Past/Coming week
  * Past/Coming month
  * Past/Coming year
* `smos-scheduler`: Fixed a bug in which relative timestamps would be scheduled
  relative to the current UTC time instead of the current local time.
* `smos-scheduler`: Fixed a bug in which items scheduled into the future would
  have future timestamps in their entries' todo state history and could
  therefore not have their todo state changed until then.
* `smos-scheduler`: Changed that scheduling rent-recurrence would happen in UTC
  time instead of in local time.
* All binaries: Changed the `--help` text to always show global options, even when a command has already been supplied.
