### Changed

* `smos-scheduler`:
  When a rent recurrence schedule is activated the first time, it is activated
  as if the current time is the time it would have been activated next.

* Home manager module: The `workflowDir` option is now put into the smos config
  file, instead of only used for the backup script.
