- `smos-data`: Made all smos file records strict. This might speed up some reports.
- `smos-scheduler`: Use SHA256 for the schedule hash so that an upgrade of the `hashable` library cannot suddenly rerun all schedules.
- `smos` and `smos-scheduler`: Made the fuzzy time parser smarter, it now also accepts:
    * "mon+1", meaning "one week after the coming monday".
    * "jan-1", meaning "january first".
