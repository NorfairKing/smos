### Added

- `smos-query`:
  The `entry` and `report` reports now support multiple output formats: `json`, `pretty-json`, and `yaml`.
  This should allow for simpler integrations.

- `smos-scheduler`:
  The `next` command to see when items will be activated next.

### Changed

- `smos`:
  Timestamps can now be deleted using the `timestampsDelete` and `timestampsRemove` actions.
- `smos-notify`:
  `smos-notify` will no longer send a notification about timestamps that don't have a time component.
- `smos-scheduler`:
  The `next` command will now print times in local time instead of UCT time. Relative times were already correct.
- `nixos-module.nix`:
  The `config` attribute will now override other attributes.

### Deleted

- `smos-server`: Removed the backup file migration looper. It went against the idea of backups.
