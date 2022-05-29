### Removed

* `smos-server`: Removed the "backup interval" feature.
  Backups are now made every time the backup looper runs, instead of only if the "backup interval" has expired.
  This means that the backup looper's period has effectively become the backup interval.
* `smos-server`: Removed the server "max backups per user" feature.
  The maximum number of backups per user _per period_ is now the default way to configure backup deletion.
  Configuring the maximum number of backups per user _over all periods_ is no longer possible.

