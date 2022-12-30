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
* `smos-query`:
  Refactored smos-report's handling of periods so that it no longer calculates
  periods at the end or beginning of a year incorrectly.
  This work has pointed out that smos-report cannot calculate periods correctly in the face of timezone changes.
  This is only a problem if you work around the hour of a timezone change often so the bug is left in so far.
* `smos-query`: Support for the following time periods:
  * Past/Coming week
  * Past/Coming month
  * Past/Coming year
