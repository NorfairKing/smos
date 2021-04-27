---
title: Changelog
description: The changelog for all of the Smos tools and libraries
---

# Unreleased Changes

## Added

- `smos`: A keybinding to open the `url` property in smos: `<space>ou` by default.
- `smos-server`: Now performs auto-backups and backup garbage-collection beyond a maximum number of backups or backup size
- `smos-server`: Now performs automatic smos file format migration for the smos files and smos backup files it stores
- `smos-server`: Admin endpoint for the admin panel
- `smos-web-server`: Admin panel with basic user information

## Changed

- `smos-github`: There is now a new column that suggests you to have another look at the issue/pr because there has been a recent enough update.
- `smos-github`: Done entries are no longer mentioned in the `list` report.
- `smos-github` Show the state of the entries in the `list` report as well.
- `smos-query` Always show "no todo state" as `(none)` instead of the empty string.
- `smos-web-server` The footer icons now no longer overlap with the terminals.

# <a name="2021-04-09">[2021-04-09 Release](#2021-04-09)

- <a name="smos-0.1.5">[smos 0.1.5](#smos-0.1.5)
- <a name="smos-api-0.2.0">[smos-api 0.2.0](#smos-api-0.2.0)
- <a name="smos-api-gen-0.1.1">[smos-api-gen 0.1.1](#smos-api-gen-0.1.1)
- <a name="smos-archive-0.2.1">[smos-archive 0.2.1](#smos-archive-0.2.1)
- <a name="smos-client-0.2.0">[smos-client 0.2.0](#smos-client-0.2.0)
- <a name="smos-convert-org-0.1.1">[smos-convert-org 0.1.1](#smos-convert-org-0.1.1)
- <a name="smos-data-0.2.1">[smos-data 0.2.1](#smos-data-0.2.1)
- <a name="smos-data-gen-0.1.2">[smos-data-gen 0.1.2](#smos-data-gen-0.1.2)
- <a name="smos-github-0.0.0">[smos-github 0.0.0](#smos-github-0.0.0)
- <a name="smos-notify-0.0.0">[smos-notify 0.0.0](#smos-notify-0.0.0)
- <a name="smos-query-0.2.3">[smos-query 0.2.3](#smos-query-0.2.3)
- <a name="smos-report-0.3.0">[smos-report 0.3.0](#smos-report-0.3.0)
- <a name="smos-report-gen-0.1.2">[smos-report-gen 0.1.2](#smos-report-gen-0.1.2)
- <a name="smos-report-cursor-0.1.2">[smos-report-cursor 0.1.2](#smos-report-cursor-0.1.2)
- <a name="smos-report-cursor-gen-0.1.2">[smos-report-cursor-gen 0.1.2](#smos-report-cursor-gen-0.1.2)
- <a name="smos-scheduler-0.1.2">[smos-scheduler 0.1.2](#smos-scheduler-0.1.2)
- <a name="smos-server-0.1.1">[smos-server 0.1.1](#smos-server-0.1.1)
- <a name="smos-server-gen-0.1.1">[smos-server-gen 0.1.1](#smos-server-gen-0.1.1)
- <a name="smos-shell-0.0.2">[smos-shell 0.0.2](#smos-shell-0.0.2)
- <a name="smos-single-0.1.1">[smos-single 0.1.1](#smos-single-0.1.1)
- <a name="smos-sync-client-0.1.2">[smos-sync-client 0.1.2](#smos-sync-client-0.1.2)
- <a name="smos-sync-client-gen-0.1.1">[smos-sync-client-gen 0.1.1](#smos-sync-client-gen-0.1.1)
- <a name="smos-web-server-0.2.0">[smos-web-server 0.2.0](#smos-web-server-0.2.0)
- <a name="smos-web-style-0.0.1">[smos-web-style 0.0.1](#smos-web-style-0.0.1)

## Added

- `smos-github`:
  [For integrating with github](/smos-github)
- `smos-server`:
  The `/api-version` endpoint for querying the API version
- `smos-client`:
  Client functions to call the `/api-version` endpoint for checking compatibility of the client with the server's API version
- `smos-notify`:
  [For integrating with desktop notifications](/smos-notify)

## Changed

- `smos-data`:
  Updated the data format to include a data format version.
  This way, we can change change the data format in the future and not get crashes.
  The previous version was already forward-compatible with this change so you should not notice this change **unless you wrote custom tooling to work with smos files**.
  The newest data-format version is version `1.0.0`.
- `smos`:
  The components of an entry are now drawn in the following order:
  1. timestamps
  1. properties
  1. contents
  1. logbook
  1. history

  where they used to be drawn in this order:
  1. contents
  1. timestamps
  1. properties
  1. history
  1. logbook
- `smos-web-server`:
  The web server now calls the API server during startup.
  This way the web server checks for both liveness of the API server and version compatibility.
- `smos-sync-client`:
  The sync client now checks compatibility with the server's API version.

## Deleted

- `smos-client-gen`: This package was empty

# <a name="2021-03-13">[2021-03-13 Release](#2021-03-13)

- <a name="smos-0.1.4">[smos 0.1.4](#smos-0.1.4)
- <a name="smos-cursor-0.2.1">[smos-cursor 0.2.1](#smos-cursor-0.2.1)
- <a name="smos-cursor-gen-0.1.1">[smos-cursor-gen 0.1.1](#smos-cursor-gen-0.1.1)
- <a name="smos-data-0.2.0">[smos-data 0.2.0](#smos-data-0.2.0)
- <a name="smos-data-gen-0.1.1">[smos-data-gen 0.1.1](#smos-data-gen-0.1.1)
- <a name="smos-query-0.2.2">[smos-query 0.2.2](#smos-query-0.2.2)
- <a name="smos-report-0.2.1">[smos-report 0.2.1](#smos-report-0.2.1)
- <a name="smos-report-cursor-0.1.1">[smos-report-cursor 0.1.1](#smos-report-cursor-0.1.1)
- <a name="smos-report-cursor-gen-0.1.1">[smos-report-cursor-gen 0.1.1](#smos-report-cursor-gen-0.1.1)
- <a name="smos-report-gen-0.1.1">[smos-report-gen 0.1.1](#smos-report-gen-0.1.1)
- <a name="smos-scheduler-0.1.1">[smos-scheduler 0.1.1](#smos-scheduler-0.1.1)
- <a name="smos-shell-0.0.1">[smos-shell 0.0.1](#smos-shell-0.0.1)
- <a name="smos-sync-client-0.1.1">[smos-sync-client 0.1.1](#smos-sync-client-0.1.1)
- <a name="smos-web-server-0.1.2">[smos-web-server 0.1.2](#smos-web-server-0.1.2)
- <a name="smos-web-style-0.0.0">[smos-web-style 0.0.0](#smos-web-style-0.0.0)

## Added

- `smos-query`:
  The 'timestamp' projection.
  You can now use `timestamp:DEADLINE` as a column in your reports.
- `smos`:
  Interactive agenda report with filter bar
- `smos`:
  Filter bar for the interactive waiting report
- `smos`:
  You can now exit any report using `Esc`, even when no file had been selected beforehand.
  Thank you [@ketzacoatl](https://github.com/ketzacoatl)!
- `smos`:
  Interactive stuck projects report
- `smos`:
  Filter bar for the built-in file browser
- `smos`:
  The `forestToggleCollapseEntryProperties` action, bound to `zp` by default.
- All commands:
  Autocompletion for the `--config-file` and `--workflow-dir` options.
- `smos`:
  The `propertiesEditProperty_goal` action, bound to `pg` by default.
- `smos`:
  Non-colour-based indicators of selection, for screens without colour or colour-blind users.
- `smos-query`:
  All reports are now bicoloured by default. This can be configured via the configuration file.
- `smos-web-server`:
  A fancy home page instead of just a login button.
  
## Changed

- `smos-data`:
  Allow spaces and parens in property values.
  Note that this means that smos files created in this new version of `smos` are potentially not backward compatible.
- `smos-scheduler`: Explicitly setting a header's `state` to `null` now leaves the generated state as null instead of setting it to `TODO`.
  Thank you [@prikhi](https://github.com/prikhi)!
- `smos-query`:
  Moved the work report warnings to above the  deadlines so that they are more in line with the order in which you should read the report.
- `smos-query` and `smos-report`:
  Moved all the static configuration of the work report out of `smos-query` into `smos-report` so that `smos` can access it for the interactive work report.
- `smos`:
  Hide properties of done entries on startup.
- `smos`:
  The waiting report is now drawn with the configured threshold, instead of always with threshold 7.




# <a name="2021-02-01">[2021-02-01 Release](#2021-02-01)

- <a name="smos-0.1.3">[smos 0.1.3](#smos-0.1.3)
- <a name="smos-query-0.1.2">[smos-query 0.1.2](#smos-query-0.1.2)
- <a name="smos-archive-0.2.0">[smos-archive 0.2.0](#smos-archive-0.2.0)


## Added

- `smos`: The `browserSelectFirst` and `browserSelectLast` actions, which are activated by `gg` and `G` respectively by default.
- `smos`: The `convArchiveFile` function, activated by `<space>fa` by default. You can use it to archive a file while looking at the file.
- Nixos module tests for home-manager configurations. The home manager module is now an official deliverable.

## Changed

- `smos-archive`: The timestamp appended to the filename during archiving now includes the local time of day instead of only the day.
- `smos`: The contents of an entry are now aligned with its state and header instead of with its list indicator.
- `smos`: Fixed a grammatical error in the description of the `entrySelectTagsFromBack` action.
- `smos`: Fixed incorrect logic with respect to displaying an ellipsis when parts of an entry were collapsed and selected.
- `smos`: Drastically reduced the idle CPU/memory usage by turning off idle GC.
- `smos`: Changed the lister character `-` to a space for a cleaner UI.
- `smos-query`: Fixed that check violations would not show up in the work report if the user had not configured any contexts.
- Converted all test suites to using [Sydtest](https://github.com/NorfairKing/sydtest) and fixed three sources of flakiness as a result.



# <a name="2020-12-16">[2020-12-16 Release](#2020-12-16)

- <a name="smos-0.1.2">[smos 0.1.2](#smos0-1.2)
- <a name="smos-calendar-import-0.2.0">[smos-calendar-import 0.2.0](#smos-calendar-import0-2.0)
- <a name="smos-cursor-0.2.0">[smos-cursor 0.2.0](#smos-cursor0-2.0)
- <a name="smos-report-0.2.0">[smos-report 0.2.0](#smos-report0-2.0)
- <a name="smos-query-0.2.0">[smos-query 0.2.0](#smos-query0-2.0)
- <a name="smos-shell-0.0.0">[smos-shell 0.0.0](#smos-shell0-0.0)
- <a name="smos-web-server-0.1.1">[smos-web-server 0.1.1](#smos-web-server0-1.1)

## Added

- `smos`: 
  You can now use Word-based movements in headers and contents.
  [cursor/#12](https://github.com/NorfairKing/cursor/pull/12)
  Thank you [@chiroptical](https://github.com/chiroptical)!
- `smos-shell`:
  A new package, to run smos commands in the browser without allowing full shell access.
- `smos`:
  It is now possible to hide timestamps.
  Timestamps of done entries are hidden by default.

## Changed

- `smos-query`:
  It is now possible to run the work report without any context.
- `smos-query`:
  The work report now also shows overdue waiting entries.
- `smos-calendar-import`:
  The importer no longer fails on malformed URIs.
- `smos-calendar-import`:
  The importer no longer fails on all other sources if any of them fails to parse.
- `smos-calendar-import`:
  The importer now has a failing exit code if one of the sources failed to parse.
- `smos-web-server`:
  The web server now sanitises output from the smos instance to make extra-sure that no non-UTF8 text gets to the client.
- `smos`:
  When pressing `?` twice, the help screen now gets closed instead of ending up in a strange empty help screen state.
- `smos-query`:
  The stuck report now also takes timestamps of not-done entries into account.
- `smos-query`:
  The stuck report now takes a `--threshold` argument.
- `smos-query`:
  The work report now shows overdue stuck projects as well.
- `smos-query`:
  The work report now requires you to configure a timewindow property if you make use of the time filter feature.

## Removed

- `smos-query`:
  The 'smart' mode for the work command has been removed. It is now the default.


# <a name="2020-11-04">[2020-11-04 Release](#2020-11-04)

- <a name="smos-0.1.1">[smos 0.1.1](#smos0-1.1)
- <a name="smos-query-0.1.1">[smos-query 0.1.1](#smos-query0-1.1)

## Added

- Static builds for all executables

## Changed

- `smos-query`:
  Make `AllTime` the default period and `OneBlock` the default block for the agenda command instead of 'Today'.
  See the Note `[Agenda command defaults]` in the code for more info.
  The default block now also changes per period. For year-based periods the default block will by month, for month-based periods the block will be by week, for week-based persiods the default block will be by day.

## Fixed

- `smos-query`:
  The agenda report now shows the "Now" line in the right place at night.
  [#164](https://github.com/NorfairKing/smos/pull/164).
  Thank you [@jecaro](https://github.com/jecaro)!
- `smos`:
  When selecting a timestamp, smos now correctly selects the value instead of the name.
  [#170](https://github.com/NorfairKing/smos/pull/170)
  Thank you [@jecaro](https://github.com/jecaro)!

## Removed

- `smos-asciinema`: This package has been entirely removed and split out to [a separate project](https://github.com/NorfairKing/autorecorder)


# <a name="2020-09-25">[2020-09-25 Release](#2020-09-25)

- <a name="smos-0.1.0">[smos 0.1.0](#smos-0.1.0)
- <a name="smos-query-0.1.0">[smos-query 0.1.0](#smos-query-0.1.0)
- <a name="smos-single-0.1.0">[smos-single 0.1.0](#smos-single-0.1.0)
- <a name="smos-archive-0.1.0">[smos-archive 0.1.0](#smos-archive-0.1.0)
- <a name="smos-asciinema-0.1.0">[smos-asciinema 0.1.0](#smos-asciinema-0.1.0)
- <a name="smos-calendar-import-0.1.0">[smos-calendar-import 0.1.0](#smos-calendar-import-0.1.0)
- <a name="smos-convert-org-0.1.0">[smos-convert-org 0.1.0](#smos-convert-org-0.1.0)
- <a name="smos-scheduler-0.1.0">[smos-scheduler 0.1.0](#smos-scheduler-0.1.0)
- <a name="smos-server-0.1.0">[smos-server 0.1.0](#smos-server-0.1.0)
- <a name="smos-sync-client-0.1.0">[smos-sync-client 0.1.0](#smos-sync-client-0.1.0)
- <a name="smos-web-server-0.1.0">[smos-web-server 0.1.0](#smos-web-server-0.1.0)

## Added

- smos: The first version of the Smos TUI
- smos-query: The first version of the Smos Query tool
- smos-single: The first version of the Smos Single tool for creating single-task projects from the command-line
- smos-archive: The first version of the Smos Arching tool for archiving Smos files
- smos-asciinema: The first version of the Smos Asciinema tool for recording ascii casts of smos demonstrations
- smos-calendar-import:  The first version of the Smos Calendar Import tool for importing calendars into your smos files
- smos-convert-org: The first version of the Smos Org-mode conversion tool for converting org-mode files to smos files
- smos-scheduler: The first version of the Smos Scheduler tool for automatically scheduling recurring projects
- smos-server: The first version of the Smos API Server
- smos-sync-client: The first version of the Smos Sync Client for synchronising smos files accross multiple devices
- smos-web-server: The first version of the Smos Web Server
