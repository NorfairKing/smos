---
title: File Format Reference
description: Documentation for the structure of a Smos file
---

A Smos file is a YAML document, for example:

``` yaml
version: 1.0.0
value:
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

It consists of a versioned forest of entries.

### Versioned forest

The top-level of a smos file consists of an object with two fields:
A `version` value which refers to the data format version.
This is used for forward and backward compatibility.
The other field is the `value` field which contains a forest of entries.

``` yaml
version: 1.0.0
value: [...]
```

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
