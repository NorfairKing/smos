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

# Reports

## Work

The work report is the most important report that you will use.
This report shows you your agenda for the day, and the next actions that you can perform in your current context.

``` plain
Usage: smos-query work CONTEXT [FILTER]
                       [--add-column|--project PROJECTION]
                       [--sort SORTER]
                       ([--hide-archived] | [-a|--show-archived])
  Show the work overview

Available options:
  CONTEXT                  The context that you are in
  FILTER                   A filter to filter entries by
  --add-column,--project PROJECTION
                           A projection to project entries onto fields
  --sort SORTER            A sorter to sort entries by
  --hide-archived          ignore archived files.
  -a,--show-archived       Don't ignore archived files.
  -h,--help                Show this help text
```

To set up a work report, you will first need contexts.
For example, you can use these in your config file:

``` yaml
contexts:
  home: 'ancestor:tag:home'
  office: 'ancestor:tag:office'
  airplane: 'tag:offline'
```

These specify that, when you're at home, you can only do the tasks that are tagged 'home' or whose parents are tagged as 'home'.


### Advanced work reports

You can also configure checks, collumns and a sorter for this report as follows:

``` yaml
work:
  checks:
    - has-property:timewindow
  columns:
    - file
    - state
    - header
    - property:timewindow
  sorter: property-as-time:timewindow
```

Here we configure `smos-query` to warn us if any of the entries that should be reported is missing a `timewindow` property.
We also specify that the collumns in the report should be the file, the todo state, the header and the `timewindow` property for each entry.
The last line configures `smos-query` to sort these entries by their `timewindow` property value.


## Agenda

``` plain
Usage: smos-query agenda [FILTER]
                         ([--historical] | [--future])
                         ([--day-block] | [--one-block])
                         ([--hide-archived] | [-a|--show-archived])
  Print the agenda

Available options:
  FILTER                   A filter to filter entries by
  --day-block              blocks of one day
  --one-block              a single block
  --hide-archived          ignore archived files.
  -a,--show-archived       Don't ignore archived files.
  -h,--help                Show this help text
```

This report shows timestamps for entries that are not market as done yet.

## Next action report

``` plain
Usage: smos-query next [FILTER] ([--hide-archived] | [-a|--show-archived])
  Print the next actions

Available options:
  FILTER                   A filter to filter entries by
  --hide-archived          ignore archived files.
  -a,--show-archived       Don't ignore archived files.
  -h,--help                Show this help text
```

This report shows you all your next actions.

## Projects

The projects report shows you an overview of all of your projects and their state.
These are the `.smos` files found in the `projects`  directory of your workflow directory.

``` plain
Usage: smos-query projects 
  Print the projects overview

Available options:
  -h,--help                Show this help text
```

## Waiting

The waiting report shows you all of the entries that you are waiting for input from others for.

``` plain
Usage: smos-query waiting [FILTER] ([--hide-archived] | [-a|--show-archived])
  Print the "waiting" tasks

Available options:
  FILTER                   A filter to filter entries by
  --hide-archived          ignore archived files.
  -a,--show-archived       Don't ignore archived files.
  -h,--help                Show this help text
```

## Clock report

The clock report shows you an overview of the clocking that you have done for each entry in a nice forest-shaped overview.

``` plain
Usage: smos-query clock [FILTER]
                        ([--begin LOCALTIME] [--end LOCALTIME]
                          | [--today]
                          | [--this-week]
                          | [--last-week]
                          | [--this-month]
                          | [--last-month]
                          | [--all-time])
                        ([--seconds-resolution]
                          | [--minutes-resolution]
                          | [--hours-resolution])
                        ([--day-block] | [--one-block])
                        ([--pretty] | [--yaml] | [--json] | [--pretty-json])
                        ([--forest] | [--flat])
                        ([--hide-archived] | [-a|--show-archived])
  Print the clock table

Available options:
  FILTER                   A filter to filter entries by
  --begin LOCALTIME        start time (inclusive)
  --end LOCALTIME          end tiem (inclusive)
  --today                  today
  --this-week              this week
  --last-week              last week
  --this-month             this month
  --last-month             last month
  --all-time               all time
  --day-block              blocks of one day
  --one-block              a single block
  --pretty                 pretty text
  --yaml                   Yaml
  --json                   single-line JSON
  --pretty-json            pretty JSON
  --hide-archived          ignore archived files.
  -a,--show-archived       Don't ignore archived files.
  -h,--help                Show this help text
```

## Log

The log report shows you an overview of the changes that have happened in a given time period.

``` plain
Usage: smos-query log [FILTER]
                      ([--begin LOCALTIME] [--end LOCALTIME]
                        | [--today]
                        | [--this-week]
                        | [--last-week]
                        | [--this-month]
                        | [--last-month]
                        | [--all-time])
                      ([--day-block] | [--one-block])
                      ([--hide-archived] | [-a|--show-archived])
  Print a log of what has happened.

Available options:
  FILTER                   A filter to filter entries by
  --begin LOCALTIME        start time (inclusive)
  --end LOCALTIME          end tiem (inclusive)
  --today                  today
  --this-week              this week
  --last-week              last week
  --this-month             this month
  --last-month             last month
  --all-time               all time
  --day-block              blocks of one day
  --one-block              a single block
  --hide-archived          ignore archived files.
  -a,--show-archived       Don't ignore archived files.
  -h,--help                Show this help text
```

## Stats

The stats report shows you an overview of some nice statistics about your workflow.

``` plain
Usage: smos-query stats ([--begin LOCALTIME] [--end LOCALTIME]
                          | [--today]
                          | [--this-week]
                          | [--last-week]
                          | [--this-month]
                          | [--last-month]
                          | [--all-time])
  Print the stats actions and warn if a file does not have one.

Available options:
  --begin LOCALTIME        start time (inclusive)
  --end LOCALTIME          end tiem (inclusive)
  --today                  today
  --this-week              this week
  --last-week              last week
  --this-month             this month
  --last-month             last month
  --all-time               all time
  -h,--help                Show this help text
```

## Tags

The tags report shows you which tags are currently in use.

``` plain
Usage: smos-query tags [FILTER]
  Print all the tags that are in use

Available options:
  FILTER                   A filter to filter entries by
  -h,--help                Show this help text
```

## Entry: generic querying

The first command to consider is the `entry` command.
It allows you to run a filter over all of your entries.

``` plain
Usage: smos-query entry [FILTER]
                        [--add-column|--project PROJECTION]
                        [--sort SORTER]
                        ([--hide-archived] | [-a|--show-archived])
  Select entries based on a given filter

Available options:
  FILTER                   A filter to filter entries by
  --add-column,--project PROJECTION
                           A projection to project entries onto fields
  --sort SORTER            A sorter to sort entries by
  --hide-archived          ignore archived files.
  -a,--show-archived       Don't ignore archived files.
  -h,--help                Show this help text
```

# Standard arguments

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
property-time:<property-name>     -- property:timewindow
reverse:<sorter>                  -- reverse:file
(<sorter> then <sorter>)          -- file then property:effort
```

## Projections

Projections allow you to select which aspects of the entries are shown.
A projection is something of the following form:

``` plain
file
state
header
tag:<tag-name>                    -- tag:toast
property:<property-name>          -- property:effort
ancestor:<property-name>          -- ancestor:effort
```
