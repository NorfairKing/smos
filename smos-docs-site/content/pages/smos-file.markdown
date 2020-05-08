---
title: Pieces of a Smos File
---

A Smos file is a YAML document, for example:

``` yaml
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
    - state: NEXT
      time: 2020-05-04 17:40:17.225761141000
  - header: Be smart about it
    state-history:
    - state: TODO
      time: 2020-05-04 17:40:25.881089668000
    tags:
    - work
```

It consists of a forest of entries.

## Entry

An entry consists of

- A header
- Contents (optionally)
- Timestamps (optionally)
- Properties (optionally)
- Its state and state history (optionally)
- Tags (optionally)
- A clock logbook (optionally)

## Header

A Smos entry's header consists of a single line of text.

Headers contain a title-like piece of information about an entry.

## Contents

A Smos entry's contents consist of an arbitrary piece of text.

Contents contain details about an entry.

## Timestamps

A Smos entry's timestamps are a map of timestamp names to timestamps.
Timestamps contain time-based information about an entry.

Timestamp names are arbitrary pieces of text without whitespace.

Standard timestamps are, for example:

- `BEGIN`
- `END`
- `SCHEDULED`
- `DEADLINE`

A timestamp has two possible granularities:

- The day level
- The moment level

## Properties

A Smos entry's properties are a map of property names to property values.
Properties contain named pieces of information about an entry.
Property names are arbitrary pieces of text without whitespace.
Property values are arbitrary pieces of text without newlines.

Properties can be used to filter entries by.

Standard properties are, for example:

- `timewindow`
- `brainpower`
- `client`


## State and state history

A Smos entry can have a state.
States contain information about the state of an entry.
States are arbitrary pieces of text without whitespace.

Tags can be used to filter entries by.

Standard states are, for example:

- `WAITING`
- `TODO`
- `STARTED`
- `READY`
- `NEXT`
- `FAILED`
- `DONE`
- `CANCELLED`

A Smos entry also remembers its entire state history.

## Tags

A Smos entry can have tags.
Tags contain label-like information about an entry.
Tags are arbitrary pieces of text without whitespace.

Properties can be used to filter entries by.

Standard tags are, for example:

- `code`
- `external`
- `home`
- `online`
- `offline`
- `power`
- `toast`

## Logbook

Each entry can contain information about when a clock was started on it.
Clocks can either be closed (have a begin and end tim.) or open (only a begin time).

Logbooks are used to generate timetables.
