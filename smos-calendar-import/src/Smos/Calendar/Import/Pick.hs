{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.Pick where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text.Lazy as LT
import Data.Time
import Smos.Calendar.Import.RecurringEvent
import Smos.Calendar.Import.Static
import Smos.Calendar.Import.TimeZone
import Smos.Calendar.Import.UnresolvedTimestamp
import qualified Text.ICalendar.Types as ICal

pickEvents :: [ICal.VCalendar] -> [RecurringEvents]
pickEvents = map pickEventsFromCalendar

pickEventsFromCalendar :: ICal.VCalendar -> RecurringEvents
pickEventsFromCalendar ICal.VCalendar {..} =
  let recurringEvents = map pickEventFromVEvent $ M.elems vcEvents
      recurringEventsTimeZones = M.map pickTimeZoneHistory $ M.mapKeys (TimeZoneId . LT.toStrict) vcTimeZones
   in RecurringEvents {..}

pickTimeZoneHistory :: ICal.VTimeZone -> TimeZoneHistory
pickTimeZoneHistory ICal.VTimeZone {..} = case S.toList (S.union vtzStandardC vtzDaylightC) of
  [tzprop] -> pickTimeZoneProp tzprop
  _ -> error "only timezones with one rule supported right now"

pickTimeZoneProp :: ICal.TZProp -> TimeZoneHistory
pickTimeZoneProp ICal.TZProp {..} =
  let timeZoneHistoryStart = case tzpDTStart of
        ICal.DTStartDateTime dt _ -> case dt of
          ICal.FloatingDateTime lt -> lt
          ICal.UTCDateTime utct -> utcToLocalTime utc utct
          ICal.ZonedDateTime lt _ -> lt -- Techincally not supported, but it'll have to be this way so we don't crash.
        ICal.DTStartDate (ICal.Date d) _ -> LocalTime d midnight -- Not allowed by the spec but it's fine.
      timeZoneHistoryOffsetFrom = pickUTCOffset tzpTZOffsetFrom
      timeZoneHistoryOffsetTo = pickUTCOffset tzpTZOffsetTo
   in TimeZoneHistory {..}

pickUTCOffset :: ICal.UTCOffset -> UTCOffset
pickUTCOffset ICal.UTCOffset {..} = UTCOffset (utcOffsetValue `div` 60)

pickEventFromVEvent :: ICal.VEvent -> RecurringEvent
pickEventFromVEvent ICal.VEvent {..} =
  let staticSummary = LT.toStrict . ICal.summaryValue <$> veSummary
      staticDescription = LT.toStrict . ICal.descriptionValue <$> veDescription
      recurringEventStatic = Static {..}
      recurringEventStart = pickStart <$> veDTStart
      recurringEventEnd = pickEndDuration <$> veDTEndDuration
   in RecurringEvent {..}

pickStart :: ICal.DTStart -> CalTimestamp
pickStart = \case
  ICal.DTStartDateTime dt _ -> CalDateTime $ pickDateTime dt
  ICal.DTStartDate d _ -> CalDate $ pickDate d

pickEndDuration :: Either ICal.DTEnd ICal.DurationProp -> CalEndDuration
pickEndDuration = \case
  Left e -> CalTimestamp $ pickEnd e
  Right d -> CalDuration $ pickDurationProp d

pickEnd :: ICal.DTEnd -> CalTimestamp
pickEnd = \case
  ICal.DTEndDateTime dt _ -> CalDateTime $ pickDateTime dt
  ICal.DTEndDate d _ -> CalDate $ pickDate d

pickDateTime :: ICal.DateTime -> CalDateTime
pickDateTime = \case
  ICal.FloatingDateTime lt -> Floating lt
  ICal.UTCDateTime utct -> UTC utct
  ICal.ZonedDateTime lt tzid -> Zoned lt (TimeZoneId (LT.toStrict tzid))

pickDate :: ICal.Date -> Day
pickDate (ICal.Date d) = d

pickDurationProp :: ICal.DurationProp -> Int
pickDurationProp (ICal.DurationProp d _) = pickDuration d

pickDuration :: ICal.Duration -> Int
pickDuration = \case
  ICal.DurationDate sign d h m s -> signNum sign * (((d * 24 + h) * 60 + m) * 60 + s)
  ICal.DurationTime sign h m s -> signNum sign * ((h * 60 + m) * 60 + s)
  ICal.DurationWeek sign w -> signNum sign * w * 7 * 24 * 60 * 60

signNum :: Num a => ICal.Sign -> a
signNum = \case
  ICal.Positive -> 1
  ICal.Negative -> -1
