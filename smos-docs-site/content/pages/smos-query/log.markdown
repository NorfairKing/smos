---
title: Using smos-query
---

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
