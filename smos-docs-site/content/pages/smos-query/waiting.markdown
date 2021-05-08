---
title: Waiting report
description: Documentation about the smos-query waiting command, for a report of what you are waiting for sorted by how long you have been waiting for it
---

The waiting report shows you all of the entries that you are waiting for input from others for.

There is a default threshold of 7 days for what "too long" means in "you have been waiting for too long, better go ping them".
You can change this default via configuration, and you can use the `waiting_threshold` property to override it on a per-entry basis.

<asciinema-player
  src="/casts/waiting.cast"
  autoplay="true"
  preloop="true"
  loop="true">
  </asciinema-player>
