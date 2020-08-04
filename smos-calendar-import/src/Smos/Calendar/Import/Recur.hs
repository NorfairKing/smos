{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.Recur where

import Smos.Calendar.Import.RecurringEvent
import Smos.Calendar.Import.UnresolvedEvent

recurEvents :: [RecurringEvents] -> [UnresolvedEvents]
recurEvents = map recurRecurringEvents

recurRecurringEvents :: RecurringEvents -> UnresolvedEvents
recurRecurringEvents RecurringEvents {..} =
  let unresolvedEvents = concatMap recurEvent recurringEvents
      unresolvedEventsTimeZones = recurringEventsTimeZones
   in UnresolvedEvents {..}

recurEvent :: RecurringEvent -> [UnresolvedEvent]
recurEvent RecurringEvent {..} =
  let unresolvedEventStatic = recurringEventStatic
      unresolvedEventStart = recurringEventStart
      unresolvedEventEnd = recurringEventEnd
   in [UnresolvedEvent {..}]
