---
title: Features
date: 2018-11-13
---

## GTD for power-users

Smos is a comprehensive system for GTD with power-users as the audience.

## Completely customisable

Smos is first and foremost a library with which you can build your own version.
It is similar to XMonad in this respect.
See [the default configuration](https://github.com/NorfairKing/smos/blob/master/smos/src/Smos/Default.hs)
for an example of a configuration.

## Future-proof file format

The Smos file format (`.smos`) is completely future proof because it is just
a subset of YAML:

![Example](../assets/smos.png)

``` smos
- entry:
    header: Use Smos
    timestamps:
      DEADLINE: 2018-10-30
      SCHEDULED: 2018-10-20
    state-history:
    - new-state: STARTED
      timestamp: 2018-10-10 14:19:53.988042844000+0000
    tags:
    - online
  forest:
  - header: Don't mess it up
    state-history:
    - new-state: DONE
      timestamp: 2018-10-10 14:19:54.388413954000+0000
  - header: Be smart about it
    state-history:
    - new-state: TODO
      timestamp: 2018-10-10 14:19:54.796427564000+0000
    tags:
    - work
```

This means that if you ever stop using smos, or it becomes unavaible,
your data is still available and easy to read.

## Machine-readible file format

The file format is very easy to work with programmatically, and convenience
functions in the `smos-data` library are provided to operate on Smos data.
You can write your own tools to operate on these files.

## Great for synchronisation

Files are made to make diffs easy and disjoint.
Derived data is never stored, but instead computed or rendered.
There is also built-in sychronisation using `smos-sync-client`.

## A replacement for emacs org mode

Smos intends to deprecate Emacs org mode by being more robust, more
customisable, by providing a better and more tool-friendly file
format and by using a more sane configuration language.

## A replacement for taskwarrior

The machine-interaction capabilities of [taskwarrior](https://taskwarrior.org)
are great, and have inspired smos greatly.
See also [TaskLite](https://tasklite.org/) for a more taskwarrior-like alternative for Smos.
