# The Smos Scheduler Tool

See https://docs.smos.online/smos-scheduler

## Technical details

### Requirements

* Stateless (other than the smos `workflow` directory)
* Still works if some schedule items are broken (missing template, syntax error, etc)
* Still works if it hasn't been run for a while
* Fast-ish

### Design

* For each schedule we remember their last activation: When it was
  activated and when it was closed, if it was.
* Rent recurrence is scheduled if the next run, after the last
  activation (or now), is before now.
* Haircut recurrence is scheduled if the next run, after the last
  closure (or now), is before now.

#### Activation history

For each schedule item in the schedule, we need to determine its
activation history based on the `workflow` directory.

Every scheduled project's first entry gets a "schedule-name"
property that tells us which schedule activated it.
Every scheduled project's first entry also gets a "schedule-activated" property that tells us the time it was activated.
When projects are archived, `smos-archive` gives them a timestamp in the path, which we use to decide when a project is closed.
