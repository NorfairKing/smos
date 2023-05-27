# The Smos Calendar Import tool

See https://docs.smos.online/smos-calendar-import


## Hacking on this:

The workflow goes:

```
ICal  -[ pick ]-> RecurringEvents       ; Pick the relevant parts out of events
      -[ recur ]-> UnresolvedEvents     ; Recur the events
      -[ resolveEdits ]-> EditedEvents  ; ResolveEdits
      -[ resolveZones ]-> [UTCEvents]   ; Resolve the events' timestamps' timezones to utc
      -[ resolveLocal ]-> [Events]      ; Resolve the events' timestamps' timezones to local time
      -[ filter ]-> [Events]            ; Filter the relevant events (time-wise)
      -[ render ]-> SmosFile            ; Render a smos file
```

## Known issues (that still need to be resolved)

The recurrence implementation operates on local times, and is unaware of
timezone resolution.  However, this can go wrong around summer/winter-time
transitions because this can introduce duplicates that are counted towards the
Count (while they shouldn't.)

We judged this to be a tiny problem because:
1. it only occurs on timezone changes
2. it only occurs on events that recur multiple times in a day AND have a 'COUNT' rule part.
