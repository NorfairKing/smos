---
title: Features
---

## Self-management for power-users

Smos is a comprehensive system for implementing self-management as a power-user.

<asciinema-player
  src="/casts/basics.cast"
  autoplay="true"
  preloop="true"
  loop="true">
  </asciinema-player>

## Completely customisable

Smos is first and foremost a library with which you can build your own version.
It is similar to XMonad in this respect.
See [the default configuration](https://github.com/NorfairKing/smos/blob/master/smos/src/Smos/Default.hs)
for an example of a configuration.

## Future-proof file format

The Smos file format (`.smos`) is completely future proof because it is just
a subset of YAML, which is plain text:

![Example smos file](/assets/smos.png)

``` smos
- entry:
    header: Use Smos
    timestamps:
      DEADLINE: 2018-10-30
      SCHEDULED: 2018-10-20
    state-history:
    - state: STARTED
      time: 2020-05-04 17:40:04.522146420000
    tags:
    - online
  forest:
  - header: Don't mess it up
    state-history:
    - state: DONE
      time: 2020-05-04 17:40:18.177282440000
    - state: TODO
      time: 2020-05-04 17:40:17.225761141000
  - header: Be smart about it
    state-history:
    - state: TODO
      time: 2020-05-04 17:40:25.881089668000
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
