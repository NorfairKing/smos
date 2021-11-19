### Added

* `convResponded`, which is like `convRespondedButStillWaiting` but does not duplicate the original entry and is bounded to `<space>rs` by default.

### Changed

* `convRespondedButStillWaiting` now also adds a `DONE` `"Respond"` task inbetween.
* Replaced `yamlparse-applicative` by `autodocodec`.
  This massively improved the documentation of configuration file formats, as well as the page about the smos file format.
  Now we can have finite schemas for recursive types, so we can show the entire schema.
