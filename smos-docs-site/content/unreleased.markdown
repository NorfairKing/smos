### Changed

* Put "ACTION REQUIRED" in the smos booking subject
* Use chunk-based deduplication for backups.
* The web-based workflow directories now use temporary directories instead of
  re-using existing directories.
  This may be slower and make it impossible to use two simultaneous sessions in
  the same directory, but at least it does not use junk on the filesystem
  anymore
* Allow any editor to edit an entry's contents instead of only `vim` and `emacs`.
