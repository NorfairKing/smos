---
title: The Smos Calendar Import Tool
description: Documentation for the Smos Calendar Import Tool, for importing your calendar into Smos
---

This tool exists to import your calendar from an external source into Smos.

<div id="cast"></div>
<script src=/assets/asciinema-player.js></script>
<script>
  AsciinemaPlayer.create('/web-assets/calendar-import.cast', document.getElementById('cast'), {
    autoPlay: true,
    preload: true,
    loop: true,
  });
</script>


Make sure to run it periodically.

Note that you can also ignore specific events while importing, by adding `SMOS_NO_CALENDAR_IMPORT` to their description.


See [the instructions for running it on NixOS](/smos-calendar-import/nixos) for automation for that on NixOS.
