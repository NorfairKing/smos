---
title: Pieces of a Smos File
description: Documentation for the structure of a Smos file
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

### Forests of trees

A forest is a list of trees.
It is parsed as a yaml list:

```
- <tree>
```

A tree is parsed as an entry with a subforest:

```
entry: <entry>
forest: <forest>
```

If a tree has no subforest, then it can be represented as just an entry:

```
<entry>
```
