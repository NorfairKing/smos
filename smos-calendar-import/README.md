# The Smos Calendar Import tool

See https://docs.smos.online/smos-calendar-import


## Hacking on this:

The workflow goes:

```
ICal  -[ pick ]-> RecurringEvents
      -[ recur ]-> UnresolvedEvents
      -[ resolve ]-> [Events]
      -[ render ]-> SmosFile
```

## Known issues (that still need to be resolved)

The recurrence implementation operates on local times, and is unaware of
timezone resolution.  However, this can go wrong around summer/winter-time
transitions because this can introduce duplicates that are counted towards the
Count (while they shouldn't.)

We judged this to be a tiny problem because:
1. it only occurs on timezone changes
2. it only occurs on events that recur multiple times in a day AND have a 'COUNT' rule part.
