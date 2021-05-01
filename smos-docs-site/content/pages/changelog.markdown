---
title: Changelog
description: The changelog for all of the Smos tools and libraries
---

# Unreleased Changes

## Changed

- `smos` and `smos-query`: More strings are now recognised as durations: `5sec`, `6 min`, `7 hours`, etc... 
  See [PR 195](https://github.com/NorfairKing/smos/pull/195) for more details.
  Thank you [@ketzacoatl](https://github.com/ketzacoatl)!
- `smos-sync-client`: Now sends the `User-Agent` header with `smos-sync-client-<version>`.
- `smos-web-server`: Now sends the `User-Agent` header with `smos-web-server-<version>`.
- `smos-sync-client`: Retry authenticating (once) if authentication fails.
  This should prevent users from having to remove the `.cache/smos/sync-session.dat` cache file when authentication fails because in the way authentication works.
- `smos-sync-client`: More descriptive error messages if login fails.
- `smos-sync-client`: Use the logger for warnings, so you can now ignore them by setting the log level higher than `LevelWarn`.
- `smos-client`: A bit of a nicer error message when something goes wrong during the client's api-version check.
- `smos-sync-client`: Use a separate lock file from the database file itself to lock the database.
  This prevents a `SQLITE_BUSY` infinite retry problem. See [Issue 202](https://github.com/NorfairKing/smos/issues/202) and [PR 203](https://github.com/NorfairKing/smos/pull/203).
  Thank you [@jhbertra](https://github.com/jhbertra)!
