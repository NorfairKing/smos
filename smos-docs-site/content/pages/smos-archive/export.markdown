---
title: Exporting your smos archive
description: Documentation for the export command of the Smos Archiving tool, for exporting your archive to cold storage.
---


When your archive directory becomes too big, this can get in the way of things like running `smos-query` commands quickly, and small periodic backups.
The `smos-archive export` command lets you export (pieces of) your archive to an external directory for cold storage.

Example usage:

``` shell
smos-archive export /path/to/export/dir --last-year --also-delete-originals
```
