---
title: Changelog
description: The changelog for all of the Smos tools and libraries
---

# Unreleased

## Added

- `smos`: The `o` key now activates the `entrySelectContentsAtEnd` key by default, `Alt-o` activates `entrySelectContentsAtStart`.
- `smos`: The `browserSelectFirst` and `browserSelectLast` actions, which are activated by `gg` and `G` respectively by default.
- `smos`: The `convArchiveFile` function, activated by ` fa` by default.

## Changed

- `smos-archive`: The timestamp appended to the filename during archiving now includes the local time of day instead of only the day.
- `smos`: The contents of an entry are now aligned with its state and header instead of with its list indicator.
- `smos`: Fixed a grammatical error in the description of the `entrySelectTagsFromBack` action.
- `smos`: Fixed incorrect logic with respect to displaying an ellipsis when parts of an entry were collapsed and selected.



# <a name="2020-12-16">[2020-12-16](#2020-12-16)

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
  Thank you [@chiroptical](https://github.com/chiroptical/)!
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


# <a name="2020-11-04">[2020-11-04](#2020-11-04)

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


# <a name="2020-09-25">[Version 0.1.0 - 2020-09-25](#2020-09-25)

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
