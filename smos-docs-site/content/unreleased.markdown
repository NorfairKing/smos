### Added

* `smos`: `convResponded`, which is like `convRespondedButStillWaiting` but does not duplicate the original entry and is bounded to `<space>rs` by default.
* `smos-calendar-import`: You can now ignore calendar events by adding `SMOS_NO_CALENDAR_IMPORT` to their description.
* `smos-scheduler`: A `sample` command to help you produce templates in a feedback loop.
* `smos`: `convUrlWaitingForReview`, a convenience function for waiting for review at a given url.
* `smos-sync-client`: Now removes empty directories after syncing, by default.
  See [its documentation](/smos-sync-client) for information about how to turn this off.
* `smos-archive`: Can now also take commands. The original use of `smos-archive <file>` still works, but can now also be invoked as `smos-archive file <file>`.
* `smos-archive`: An `export` command that lets you export pieces of your archive for cold storage.

### Changed

* `smos`: `convRespondedButStillWaiting` now also adds a `DONE` `"Respond"` task inbetween.
* Replaced `yamlparse-applicative` by `autodocodec`.
  This massively improved the documentation of configuration file formats, as well as the page about the smos file format.
  Now we can have finite schemas for recursive types, so we can show the entire schema.
* Upgraded `validity` dependency to beyond `genvalidity >=1.0.0.0`.
* `smos`: Smarter starter unfolding rule. 
  Before, smos would go to the last deepest entry as a proxy for the "current" entry for a project.
  After this change, smos goes to the first, deepest, not-done entry.
  Any entirely-done subforests will also be collapsed on the way there.

### Removed

* The static builds. Even if anyone were to use them, and I don't think anyone did, I don't think they actually worked because of the way terminfo is dealt with.
