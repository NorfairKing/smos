### Changed

* Fixed that the downloads were not statically linked on the dynamically linked docs site.
* Sped up backup garbage collection using cascading deletions.
* The server now only makes an automatic backup if there could be updated
  files.
  This takes some load off the server because it will no longer make backups
  for unused accounts.
* Renamed the systemd services for hosting a server to group them by environment name.
