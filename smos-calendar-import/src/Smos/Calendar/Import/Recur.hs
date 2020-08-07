{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.Recur where

import qualified Data.Set as S
import Data.Time
import Smos.Calendar.Import.RecurrenceRule
import Smos.Calendar.Import.RecurringEvent
import Smos.Calendar.Import.UnresolvedEvent
import Smos.Calendar.Import.UnresolvedTimestamp

recurEvents :: [RecurringEvents] -> [UnresolvedEvents]
recurEvents = map recurRecurringEvents

recurRecurringEvents :: RecurringEvents -> UnresolvedEvents
recurRecurringEvents RecurringEvents {..} =
  let unresolvedEvents = concatMap recurEvent recurringEvents
      unresolvedEventsTimeZones = recurringEventsTimeZones
   in UnresolvedEvents {..}

recurEvent :: RecurringEvent -> [UnresolvedEvent]
recurEvent RecurringEvent {..} = do
  let unresolvedEventStatic = recurringEventStatic
  rrule <- S.toList recurringEventRRules -- For each rrule
  (unresolvedEventStart, unresolvedEventEnd) <- recurMUnresolvedTimestamps rrule recurringEventStart recurringEventEnd
  pure UnresolvedEvent {..}

recurMUnresolvedTimestamps :: RRule -> Maybe CalTimestamp -> Maybe CalEndDuration -> [(Maybe CalTimestamp, Maybe CalEndDuration)]
recurMUnresolvedTimestamps rrule mstart mend = case (mstart, mend) of
  (Nothing, Nothing) -> [(Nothing, Nothing)] -- One occurrence, just to make sure we don't miss any events even if they're weird...
  (Just start, Nothing) -> (,) <$> (Just <$> recurCalTimestamp rrule start) <*> pure Nothing
  (Nothing, Just end) -> (,) Nothing <$> (Just <$> recurCalEndDuration rrule end)
  (Just start, Just end) -> map (\(a, b) -> (Just a, Just b)) $ recurUnresolvedTimestamps rrule start end

-- FIXME This is probably not correct in the case where any of the timestamps are invalid and therefore skipped
-- To fix this, we probably want to build in an 'INVALID' kind of thing into the recurrence rule calculation.
recurUnresolvedTimestamps :: RRule -> CalTimestamp -> CalEndDuration -> [(CalTimestamp, CalEndDuration)]
recurUnresolvedTimestamps rrule start end = case end of
  CalDuration i -> (,) <$> recurCalTimestamp rrule start <*> pure (CalDuration i)
  CalTimestamp endTS -> zip (recurCalTimestamp rrule start) (CalTimestamp <$> recurCalTimestamp rrule endTS)

recurCalEndDuration :: RRule -> CalEndDuration -> [CalEndDuration]
recurCalEndDuration rrule = \case
  CalTimestamp cts -> CalTimestamp <$> recurCalTimestamp rrule cts
  CalDuration i -> [CalDuration i]

recurCalTimestamp :: RRule -> CalTimestamp -> [CalTimestamp]
recurCalTimestamp rrule = \case
  CalDateTime cdt -> CalDateTime <$> recurCalDateTime rrule cdt
  CalDate d -> undefined

recurCalDateTime :: RRule -> CalDateTime -> [CalDateTime]
recurCalDateTime rrule = \case
  Floating lt -> Floating <$> recurLocalTime rrule lt
  UTC utct -> UTC <$> recurUTCTime rrule utct
  Zoned lt tzid -> Zoned <$> recurLocalTime rrule lt <*> pure tzid

recurUTCTime :: RRule -> UTCTime -> [UTCTime]
recurUTCTime rrule utct = do
  let lt = utcToLocalTime utc utct
  lt' <- recurLocalTime rrule lt
  pure $ localTimeToUTC utc lt'

recurLocalTime :: RRule -> LocalTime -> [LocalTime]
recurLocalTime rrule lt = S.toList $ rruleOccurrencesUntil lt rrule undefined
