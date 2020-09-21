---
title: The Smos Scheduler Tool
---

The `smos-scheduler` tool is configured most easily using a configuration file.

Example configuration:

``` yaml
scheduler:
  schedule:
    - description: "Weekly tasks"
      template: templates/weekly.smos.template
      destination: projects/weekly-[ %V | monday ].smos
      schedule: "0 12 * * 7" # Cron schedule: "At 12:00 on Sunday."
```

Example template at `templates/weekly.smos.template`:

``` yaml
- entry: Weekly actions
  forest:
  - header: Weekly review
    state: READY
    timestamps:
      SCHEDULED: "[ %F | saturday ]"
```


Run `smos-scheduler check` to make sure everything is ready for the template to be scheduled correctly.

Then run `smos-scheduler schedule` periodically.
Once per hour or once per day should be enough but make sure to automate it.


## Templating language

The destination filename as well as the template of the smos file itself use a templating language.
Most of the contents are just literal text, but timestamps can be spliced in as necesary.

Splicing works using the `[ template ]` or `[ template | fuzzy timestamp ]` syntax.
In practice, `[ template ]` is sugar for `[ template | time at which the scheduler is run]`.

Some examples:

* On 2020-07-19, `[ %F ]` becomes `2020-07-19`.
* On 2020-07-19, `[ %W ]` becomes `29`. (`%W` represents the week number)
* On 2020-07-19, `[ %F | monday ]` becomes `2020-07-20`.
* On 2020-07-19, `[ %W | monday ]` becomes `30`. (`%W` represents the week number)

