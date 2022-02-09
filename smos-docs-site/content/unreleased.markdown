
### Added

* New `state` and `header` sorters

### Changed

* `smos-calendar-import`: Import sources in parallel, instead of one by one.
* `smos-calendar-import`: Render the event `UID` when `--debug` is used.
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
