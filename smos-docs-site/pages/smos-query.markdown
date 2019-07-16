---
title: Using smos-query
date: 2018-12-03
---

The `smos` editor comes with some handy companion tools.
`smos-query` is one of those and it is a query tool for your `.smos` files.

# File structure

The `smos-query` tool operates in what is called the workflow directory.
This is `~/workflow` by default, but can be overridden using a flag, an environment variable or in a configuration file.

The `smos-query` tool will only consider the contents of the workflow directory for its functionality.

# Entry: generic querying

The first command to consider is the `entry` command.
It allows you to run a filter over all of your entries.

```
smos-query entry [FILTER] [--project PROJECTION] [--sort SORTER]
```

Filters allow you to select which entries are shown.
A filter is something of the following form:

```
tag:<tag>                                          -- tag:online
state:<state>                                      -- state:TODO
file:<file>                                        -- file:my-client.smos
level:<level>                                      -- level:5
property:has:<property-name>                       -- property:has:effort
property:exact:<property-name>:<property-value>    -- property:exact:effort:15min
parent:<filter>                                    -- parent:tag:work
ancestor:<filter>                                  -- ancestor:state:DONE
child:<filter>                                     -- child:tag:work
legacy:<filter>                                    -- legacy:state:DONE
not:<filter>                                       -- not:tag:work
(<filter> and <filter>)                            -- (tag:work and state:NEXT)
(<filter> or <filter>)                             -- (level:0 or state:DONE)
```

Projections allow you to select which aspects of the entries are shown.
A projection is something of the following form:

```
file
state
header
property:<property-name>          -- property:effort
(<projection> also <projection>)  -- file also state
```

Sorters allow you to select in which order the entries are shown.
A sorter is something of the following form:

```
file
property:<property-name>          -- property:effort
reverse:<sorter>                  -- reverse:file
(<sorter> then <sorter>)          -- file then property:effort
```

# Agenda

The first important command to know about is the agenda report:

```
smos-query agenda [FILTER]
```

This report shows timestamps for entries that are not market as done yet.

# Next action report

The second important command is the next action report:

```
smos-query next [FILTER]
```

This report shows you your next actions

# Projects

The projects report shows you an overview of all of your projects.
These are the `.smos` files found in the `projects`  directory of your workflow directory.

```
smos-query projects
```

# Waiting

The waiting report shows you all of the entries that you are waiting for input from others for.

```
smos-query waiting [FILTER]
```

# Clock report

The clock report shows you an overview of the clocking that you have done for each entry in a nice forest-shaped overview.

```
smos-query clock [FILTER]
```

# Log

The log report shows you an overview of the changes that have happened in a given time period.

```
smos-query log [FILTER]
```

# Stats

The stats report shows you an overview of some nice statistics about your workflow.

```
smos-query stats [FILTER]
```
