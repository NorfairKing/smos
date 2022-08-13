---
title: The Smos Scheduler Tool
description: Documentation for the Smos Scheduler Tool, for scheduling projects on a recurring basis
---

The `smos-scheduler` tool can be used to schedule recurring projects.
It is configured declaratively using a configuration file.

A scheduled project is configured to be activated on a given schedule.
An activation consists of filling in a template project (with extension `.smos.template`) and putting the result at a given destination as a `.smos` file.


### Configuration

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

See below for the reference documentation about configuration.

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

See [the template format reference documentation](/smos-scheduler/template) for instructions on how to write a template.

### Usage

Run `smos-scheduler check` to make sure everything is ready for the template to be scheduled correctly.

Run `smos-scheduler next` to see when your items will next be activated.

Then run `smos-scheduler schedule` periodically.
Once per hour or once per day should be enough but make sure to automate it.

### Types of recurrence

Recurrence falls into two categories: rent recurrence and haircut recurrence.
The difference between the two lies in when the next instance should be scheduled if one overruns.
In the case of rent recurrence, for example on the first day of every month, the next instance should be scheduled on the same day irrespective of whether rent was late last month.
In the case of haircut recurrence, the situation is different.
When your most recent haircut was late, the next one should not be any sooner after that.

Examples of rent recurrence:

* Paying rent
* Sending invoices
* Paying taxes

Examples of haircut recurrence:

* Getting a haircut
* Watering plants
* Getting a massage
* Seeing a dentis
* Cleaning your home

The `smos-scheduler` tool supports both of these types of recurrence.
Rent recurrence is configured using a [cron schedule](https://en.wikipedia.org/wiki/Cron#Overview), and haircut recurrence is configured using an amount of time.
