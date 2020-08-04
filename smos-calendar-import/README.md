# Smos calendar import


## Hacking on this:

The workflow goes:

```
ICal  -[ pick ]-> RecurringEvents
      -[ recur ]-> UnresolvedEvents
      -[ resolve ]-> [Events]
```
