### Changed

* Fixed that the `smos-github` config was not included in the config file
  generated in the home manager module.
* Refactored most of the `smos-calendar-import` logic into [a separate `ical` library](https://github.com/NorfairKing/ical).
  This means that:
  1. Calendar imports should be more accurate now; in particular around timezone changes.
  2. Smos can now more gracefully handle services outputting invalid `.ics` files.
     Indeed, Google, Apple, and Microsoft all sometimes spit out invalid `.ics` files.
