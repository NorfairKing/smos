---
title: Archiving a single file
description: Documentation for the file command of the Smos Archiving tool, for archiving a single smos file
---

You can archive files that you are done with.
This way it can still be considered for reports using `smos-query`, but it is not in the way when looking at the rest of your files.

To archive a file, run `smos-archive <file>` and it will be moved to the archive dir.
Any non-done entries will also be marked as `CANCELLED`.

<div id="cast"></div>
<script src=/assets/asciinema-player.js></script>
<script>
  AsciinemaPlayer.create('/casts/archive.cast', document.getElementById('cast'), {
    autoPlay: true,
    preload: true,
    loop: true,
  });
</script>

