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
smos-query entry [FILTER]
```

A filter is something of the following form:

```
tag:<tag>                -- tag:online
state:<state>            -- state:TODO
file:<file>              -- file:my-client.smos
level:<level>            -- level:5
parent:<filter>          -- parent:tag:work
ancestor:<filter>        -- ancestor:state:DONE
not:<filter>             -- not:tag:work
(<filter> and <filter>)  -- (tag:work and state:NEXT)
(<filter> or <filter>)   -- (level:0 or state:DONE)
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
