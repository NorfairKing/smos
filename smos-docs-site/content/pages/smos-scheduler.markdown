---
title: The Smos Scheduler Tool
description: Documentation for the Smos Scheduler Tool, for scheduling projects on a recurring basis
---

The `smos-scheduler` tool is configured declaratively using a configuration file.

Example configuration:

``` yaml
scheduler:
  schedule:
    - description: "Rent"
      template: templates/rent.smos.template
      destination: projects/rent-[ %m | 1 ].smos
      schedule: "0 12 16 * *" # Cron schedule: "At 12:00 on the 16th day of the month"
    - decscription: "Haircut
      template: templates/haircut.smos.template
      destination: projects/haircut.smos
      schedule: 1 month # One month after the provious haircut project has been completed.
```

Example template at `templates/rent.smos.template`:

``` yaml
- entry:
    header: Rent
    properties:
      goal: Pay rent on time
  forest:
  - header: Pay rent
    state: READY
    timestamps:
      SCHEDULED: "[ %F | 25 ]"
```


Run `smos-scheduler check` to make sure everything is ready for the template to be scheduled correctly.
Run `smos-scheduler next` to see when your items will next be activated.

Then run `smos-scheduler schedule` periodically.
Once per hour or once per day should be enough but make sure to automate it.

Note that schedules are expressed in UTC time, so using `"0 12 * * 7"` means `"At 12:00 on Sunday."` _in UTC time_.

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

