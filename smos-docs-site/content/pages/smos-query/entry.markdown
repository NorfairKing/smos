---
title: Entry report: generic querying
---

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

