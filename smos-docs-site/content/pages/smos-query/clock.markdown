---
title: Using smos-query
---

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
