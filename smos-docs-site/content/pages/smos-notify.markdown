---
title: The Notification Tool
description: Documentation for the Smos Notification Tool, for sending desktop notifications about entries
---

This tool exists to send desktop notifications about events.

The tool should be run every minute to notify you in time.
It will still work as-expected if it is run late, but you might miss notifications if an event has already started by the time the tool is run, for example.

## Dependencies

The `smos-notify` tool will use the `notify-send` tool from `libnotify` to send notifications.
This means that you need to have set up a notification server.
(If you don't know what that is, chances are that it's already set up for you.)
See [the relevant page on the arch wiki](https://wiki.archlinux.org/index.php/Desktop_notifications) for more information.

The `smos-notify` tool will also use `play` from `sox` to play a sound when there are any notifications.
If `play` is not available, it will still send notifications but not play any sound.
