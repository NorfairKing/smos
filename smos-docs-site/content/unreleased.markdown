##### Added

* `smos-server`: Now supports logging, and some logging has already been added as well.
* `smos`:
  The `convCopyContentsToClipboard` action to copy the selected entry's contents to the system clipboard.
  This fixes [issue 205](https://github.com/NorfairKing/smos/issues/205).
  Thank you [@distefam](https://github.com/distefam)!
   

##### Changed

* `smos-calendar-import`: The rendered smos entries now all contain the description of the event as contents, instead of only the top-level event.
* `smos-notify`: You can now put the magic string `SMOS_NO_NOTIFY` into event descriptions to have `smos-notify` ignore the event entirely.
* `smos-sync-client`:
  Now sends the username of the user and the hostname of the device that makes the requests in the `Referer` header of every request.
  This information is only used for logging.
* `smos-web-server`: Now requires a `WEB_URL` to be configured and sends it over in the `Referer` header of every request to the API.
* `smos`: Fixed that the file browser filter was shown in an empty file browser as well.
* `smos-query`: Added a header to each of the columns in the waiting report.
* `smos` and `smos-query`: Made the filepaths not as prominent, visually.
* `smos` and `smos-query`: Made report formatting more consistent
* `smos`: No longer shows the `.smos` extension for every stuck project in the stuck projects report.
* `smos` and `smos-query`: Allow thresholds for the waiting, stuck and work reports to be configured as more general time strings.
* `smos` and `smos-query`: Added the `waiting_threshold` to allow for a custom per-entry waiting threshold.
* `smos`: Fixed that the filter bar wasn't shown in the interactive work report.
  This fixes [issue 206](https://github.com/NorfairKing/smos/issues/206).
  Thank you [@vidocco](https://github.com/vidocco)!
