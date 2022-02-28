
### Added

* New `state` and `header` sorters

### Changed

* `smos-calendar-import`: Import sources in parallel, instead of one by one.
* `smos-calendar-import`: Render the event `UID` as a property and the original event in contents when `--debug` is used.
* `smos`: When opening a file, the uncollapse rule has been changed to skip over entries without a state.

  Before:

  ```
  Top
    DONE something done
  ❯ some info with no state 
    NEXT thing to do
  ```

  After:

  ```
  Top
    DONE something done
    some info with no state 
  ❯ NEXT thing to do
  ```
* `smos`: Contents with carriage return or tab characters can now be shown in smos correctly.
* `smos-report`: Fixed a bug where some filters could not be parsed because they contained substrings `"or"` or `"and"`.
* Home manager module: Fixed the naming of systemd services to be prefixed by `smos-` for easier tab-completion.
* `smos-data`: Simplified time-related data parsing.
* `smos-data`: Upgraded to data format `2.0.0`, which allows timestamps to be less precise.
* `smos-scheduler`: Use atomic durable writes for the state, so that the state never gets lost when something goes wrong during a write.
* `smos-sync-client`: Use atomic durable writes for the login session, so that the session never gets lost when something goes wrong during a write.
