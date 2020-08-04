{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.Pick where

import Control.Monad.Reader
import qualified Data.Map as M
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Data.Time
import Smos.Calendar.Import.RecurringEvent
import Text.ICalendar.Types as ICal

pickEvents :: [ICal.VCalendar] -> [RecurringEvents]
pickEvents = map pickEventsFromCalendar

pickEventsFromCalendar :: ICal.VCalendar -> RecurringEvents
pickEventsFromCalendar ICal.VCalendar {..} =
  let recurringEvents = map pickEventFromVEvent $ M.elems vcEvents
      recurringEventsTimeZones = M.map pickTimeZoneHistory $ M.mapKeys (TimeZoneId . LT.toStrict) vcTimeZones
   in RecurringEvents {..}

pickTimeZoneHistory :: VTimeZone -> TimeZoneHistory
pickTimeZoneHistory = undefined

pickEventFromVEvent :: ICal.VEvent -> RecurringEvent
pickEventFromVEvent ICal.VEvent {..} =
  let recurringEventSummary = LT.toStrict . ICal.summaryValue <$> veSummary
      recurringEventDescription = LT.toStrict . ICal.descriptionValue <$> veDescription
      recurringEventStart = pickStart <$> veDTStart
      recurringEventEnd = pickEndDuration <$> veDTEndDuration
   in RecurringEvent {..}

pickStart :: ICal.DTStart -> CalTimestamp
pickStart = \case
  DTStartDateTime dt _ -> CalDateTime $ pickDateTime dt
  DTStartDate d _ -> CalDate $ pickDate d

pickEndDuration :: Either ICal.DTEnd ICal.DurationProp -> CalEndDuration
pickEndDuration = \case
  Left e -> CalTimestamp $ pickEnd e
  Right d -> CalDuration $ pickDurationProp d

pickEnd :: ICal.DTEnd -> CalTimestamp
pickEnd = \case
  DTEndDateTime dt _ -> CalDateTime $ pickDateTime dt
  DTEndDate d _ -> CalDate $ pickDate d

pickDateTime :: ICal.DateTime -> CalDateTime
pickDateTime = \case
  FloatingDateTime lt -> Floating lt
  UTCDateTime utct -> UTC utct
  ZonedDateTime lt tzid -> Zoned lt (TimeZoneId (LT.toStrict tzid))

pickDate :: ICal.Date -> Day
pickDate (Date d) = d

pickDurationProp :: ICal.DurationProp -> Int
pickDurationProp (DurationProp d _) = pickDuration d

pickDuration :: ICal.Duration -> Int
pickDuration = \case
  DurationDate sign d h m s -> signNum sign * (((d * 24 + h) * 60 + m) * 60 + s)
  DurationTime sign h m s -> signNum sign * ((h * 60 + m) * 60 + s)
  DurationWeek sign w -> signNum sign * w * 7 * 24 * 60 * 60

signNum :: Num a => ICal.Sign -> a
signNum Positive = 1
signNum Negative = -1
