---
title: Standard arguments to smos-query commands
description: Documentation about the standard arguments that you can pass to multiple of the smos-query commands
---

## Filters

Filters allow you to select which entries are shown.
A filter is something of the following form:

``` plain
tag:<tag>                                          -- tag:online
state:<state>                                      -- state:TODO
file:<file>                                        -- file:my-client.smos
level:<level>                                      -- level:5
header:<header>                                    -- header:find
property:<property-name>                           -- property:effort
property:<property-name>:<property-value>          -- property:effort:high
property:<property-name>:time:<comparison>:<time>  -- property:timewindow:time:lt:2h
parent:<filter>                                    -- parent:tag:work
ancestor:<filter>                                  -- ancestor:state:DONE
child:<filter>                                     -- child:tag:work
legacy:<filter>                                    -- legacy:state:DONE
not:<filter>                                       -- not:tag:work
(<filter> and <filter>)                            -- (tag:work and state:NEXT)
(<filter> or <filter>)                             -- (level:0 or state:DONE)
```

## Sorters

Sorters allow you to select in which order the entries are shown.
A sorter is something of the following form:

``` plain
file
property:<property-name>          -- property:effort
property-as-time:<property-name>  -- property-as-time:timewindow
reverse:<sorter>                  -- reverse:file
(<sorter> then <sorter>)          -- file then property:effort
```

## Projections (Columns)

Projections allow you to select which aspects of the entries are shown.
They usually represent the columns in a report.
A projection is something of the following form:

``` plain
file
state
header
tag:<tag-name>                    -- tag:toast
property:<property-name>          -- property:effort
timestamp:<timestamp-name>        -- timestamp:DEADLINE
ancestor:<projection>             -- ancestor:tag:toast
```
