---
title: The Smos Archiving tool
---

You can archive files that you are done with.
This way it can still be considered for reports using `smos-query`, but it is not in the way when looking at the rest of your files.

To archive a file, run `smos-archive <file>` and it will be moved to the archive dir.
Any non-done entries will also be marked as `CANCELLED`.

<asciinema-player
  src="/casts/archive.cast"
  rows="25"
  cols="80"
  autoplay="true"
  preloop="true"
  loop="true">
  </asciinema-player>

