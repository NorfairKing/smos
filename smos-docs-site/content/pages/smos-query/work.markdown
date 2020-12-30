---
title: Work report
description: Documentation about the smos-query work command, for the contextual work report
---

The work report is the most important report that you will use.
This report shows you your agenda for the day, and the next actions that you can perform in your current context, as well as any warnings that you need to see about the state of your workflow.

<asciinema-player
  src="/casts/work.cast"
  autoplay="true"
  preloop="true"
  loop="true">
  </asciinema-player>

## Contexts

For context-based work reports, you will first need contexts.
For example, you can use these in your config file:

``` yaml
work:
  contexts:
    home: 'ancestor:tag:home'
    office: 'ancestor:tag:office'
    airplane: 'tag:offline'
```

These specify that, when you're at home, you can only do the tasks that are tagged 'home' or whose parents are tagged as 'home'.


## Advanced work reports

You can also configure checks, collumns and a sorter for this report as follows:

``` yaml
work:
  checks:
    - 'property:timewindow'
  columns:
    - 'file'
    - 'state'
    - 'header'
    - 'property:timewindow'
  sorter: 'reverse:property-as-time:timewindow'
```

Here we configure `smos-query` to warn us if any of the entries that should be reported is missing a `timewindow` property.
We also specify that the collumns in the report should be the file, the todo state, the header and the `timewindow` property for each entry.
The last line configures `smos-query` to sort these entries by their `timewindow` property value.
(Checks, columns and sorters are [Standard Arguments](/smos-query/standard-arguments).)

