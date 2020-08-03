{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.Recur where

import Data.Time
import Smos.Calendar.Import.Event
import Smos.Calendar.Import.RecurringEvent
import Smos.Data

recurEvents :: TimeZone -> [RecurringEvent] -> [Event]
recurEvents tz = concatMap (recurEvent tz)

recurEvent :: TimeZone -> RecurringEvent -> [Event]
recurEvent tz RecurringEvent {..} =
  [ let eventSummary = recurringEventSummary
        eventDescription = recurringEventDescription
        eventStart = recurStart tz <$> recurringEventStart
        eventEnd = recurringEventEnd >>= recurEndDuration tz eventStart
     in Event {..}
  ]

recurStart :: TimeZone -> CalTimestamp -> Timestamp
recurStart = recurTimestamp

recurDateTime :: TimeZone -> CalDateTime -> LocalTime
recurDateTime tz = \case
  Floating lt -> lt
  UTC lt -> utcToLocalTime utc lt
  Zoned lt (UTCOffset u) ->
    let utct = localTimeToUTC (minutesToTimeZone (u * 60)) lt
     in utcToLocalTime tz utct

recurEndDuration :: TimeZone -> Maybe Timestamp -> CalEndDuration -> Maybe Timestamp
recurEndDuration tz mstart = \case
  CalTimestamp ts -> Just $ recurTimestamp tz ts
  CalDuration ndt -> do
    start <- mstart
    let lt = timestampLocalTime start
    pure $ TimestampLocalTime $ addLocalTime (fromIntegral ndt) lt

recurTimestamp :: TimeZone -> CalTimestamp -> Timestamp
recurTimestamp tz = \case
  CalDate d -> TimestampDay d
  CalDateTime dt -> TimestampLocalTime $ recurDateTime tz dt
