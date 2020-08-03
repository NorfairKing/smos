{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.Recur where

import Data.Validity
import GHC.Generics
import Smos.Calendar.Import.Event
import Smos.Calendar.Import.RecurringEvent
import Text.ICalendar.Types

recurEvents :: [RecurringEvent] -> [Event]
recurEvents = concatMap recurEvent

recurEvent :: RecurringEvent -> [Event]
recurEvent RecurringEvent {..} =
  [ let eventSummary = recurringEventSummary
        eventDescription = recurringEventDescription
     in Event {..}
  ]
