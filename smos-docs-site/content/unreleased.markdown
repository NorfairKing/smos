- <a name="smos-archive-0.7.1">[smos-archive 0.7.1](#smos-archive-0.7.1)
- <a name="smos-scheduler-0.10.0">[smos-scheduler 0.10.0](#smos-scheduler-0.10.0)

### Changed

* `smos-scheduler`: Fixed that late-schedulings would be scheduled with the wrong time.
* `smos-scheduler`: Use a unique name instead of hash so that schedules no longer need to be rescheduled when they are changed.
* `smos-scheduler`: Redesigned the way schedules are configured to require a name.
   This is a breaking change and will require a (very mechanical) change to your configuration.
