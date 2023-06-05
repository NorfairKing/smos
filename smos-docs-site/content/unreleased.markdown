### Added

* `smos`: `pp` for setting the `phone_number` property.
* `smos-jobhunt`: Made the waiting threshold configurable.
* `smos-calendar-import`: Now records whether you should be considered busy during an event in the `busy` property.
* `smos-query`: The `free` command: For showing when you may be available.
* `smos-query`: The `tags` command got a `--show-archive` flag.
* `smos`: The `AFTER` timestamp:
   Don't show next actions if now is before the 'AFTER' timestamp.
   Don't show deadlines if now is before the 'AFTER' timestamp.
* `smos-server`: Booking API: Users can now activate booking and be booked.
* `smos-web-server`: Booking UI: Users can now activate booking and be booked.

### Changed

* `smos-jobhunt`: Fix that the url wasn't being filled in.
* `smos`: Fixed that the `email` property was not coloured.
* `smos-jobhunt`: Fixed that the sender didn't receive a copy of the email.
* `smos-jobhunt`: Made the default waiting threshold 3 weeks.
* `smos-query`: The `tags` command now hides the archive by default.
* `smos-query`: Sped up the work report generation slightly by using difference lists.
* `smos-calendar-import`: Updated the `ical` depnedency.
  This should speed up parsing of large calendars.
  It should also allow importing from ICloud where previously that was not
  possible because ICloud outputs invalid iCalendar files.
* `smos-query`: Don't consider entries with the property `busy: false` in the "next begin" part of the work report.
* `smos-api`: Bumped the API version to `0.4` with the addition of the booking API endpoints.
* All packages: Upgraded to LTS 20.23 and nixpkgs branch `nixos-23.05`.
