{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.Pick where

import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Text.Lazy as LT
import Data.Time
import Smos.Calendar.Import.RecurrenceRule
import Smos.Calendar.Import.RecurringEvent
import Smos.Calendar.Import.Static
import Smos.Calendar.Import.TimeZone
import Smos.Calendar.Import.UnresolvedTimestamp
import qualified Text.ICalendar.Types as ICal

pickEvents :: [ICal.VCalendar] -> [RecurringEvents]
pickEvents = map pickEventsFromCalendar

pickEventsFromCalendar :: ICal.VCalendar -> RecurringEvents
pickEventsFromCalendar ICal.VCalendar {..} =
  let recurringEvents = mapMaybe pickEventFromVEvent $ M.elems vcEvents
      recurringEventsTimeZones = M.map pickTimeZoneHistory $ M.mapKeys (TimeZoneId . LT.toStrict) vcTimeZones
   in RecurringEvents {..}

pickTimeZoneHistory :: ICal.VTimeZone -> TimeZoneHistory
pickTimeZoneHistory ICal.VTimeZone {..} = TimeZoneHistory $ map pickTimeZoneProp $ S.toList (S.union vtzStandardC vtzDaylightC)

pickTimeZoneProp :: ICal.TZProp -> TimeZoneHistoryRule
pickTimeZoneProp ICal.TZProp {..} =
  let timeZoneHistoryRuleStart = case tzpDTStart of
        ICal.DTStartDateTime dt _ -> case dt of
          ICal.FloatingDateTime lt -> lt
          ICal.UTCDateTime utct -> utcToLocalTime utc utct
          ICal.ZonedDateTime lt _ -> lt -- Techincally not supported, but it'll have to be this way so we don't crash.
        ICal.DTStartDate (ICal.Date d) _ -> LocalTime d midnight -- Not allowed by the spec but it's fine.
      timeZoneHistoryRuleOffsetFrom = pickUTCOffset tzpTZOffsetFrom
      timeZoneHistoryRuleOffsetTo = pickUTCOffset tzpTZOffsetTo
      timeZoneHistoryRuleRRules = pickRRule tzpRRule
   in TimeZoneHistoryRule {..}

pickUTCOffset :: ICal.UTCOffset -> UTCOffset
pickUTCOffset ICal.UTCOffset {..} = UTCOffset (utcOffsetValue `div` 60)

pickEventFromVEvent :: ICal.VEvent -> Maybe RecurringEvent
pickEventFromVEvent ICal.VEvent {..} =
  let staticSummary = LT.toStrict . ICal.summaryValue <$> veSummary
      staticDescription = case LT.toStrict . ICal.descriptionValue <$> veDescription of
        Nothing -> Nothing
        Just "" -> Nothing -- Don't pick the empty string, it's pointless.
        Just d -> Just d
      recurringEventStatic = Static {..}
      recurringEventStart = pickStart <$> veDTStart
      recurringEventEnd = pickEndDuration <$> veDTEndDuration
      recurringEventRRules = pickRRule veRRule
   in case veStatus of
        Just (ICal.CancelledEvent _) -> Nothing -- Don't pick cancelled events
        _ -> Just RecurringEvent {..}

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

-- FIXME: This can fail in many ways if the rule wasn't valid; should we watch out for that?
pickRRule :: Set ICal.RRule -> Set RRule
pickRRule = S.map $ \ICal.RRule {..} ->
  let ICal.Recur {..} = rRuleValue
      rRuleFrequency = case recurFreq of
        ICal.Secondly -> Secondly
        ICal.Minutely -> Minutely
        ICal.Hourly -> Hourly
        ICal.Daily -> Daily
        ICal.Weekly -> Weekly
        ICal.Monthly -> Monthly
        ICal.Yearly -> Yearly
      rRuleInterval = Interval $ fromIntegral recurInterval
      rRuleUntilCount = case recurUntilCount of
        Nothing -> Indefinitely
        Just untilCount -> case untilCount of
          Left (Left d) -> Until (LocalTime (ICal.dateValue d) midnight) -- This might run us into trouble, but I think it _shouldn't_ if the rule was valid.
          Left (Right dt) -> case dt of -- This _should_ work. Let's see.
            ICal.FloatingDateTime lt -> Until lt
            ICal.UTCDateTime utct -> Until $ utcToLocalTime utc utct
            ICal.ZonedDateTime lt _ -> Until lt
          Right i -> Count $ fromIntegral i
      rRuleBySecond = S.fromList $ map (Second . fromIntegral) recurBySecond
      rRuleByMinute = S.fromList $ map (Minute . fromIntegral) recurByMinute
      rRuleByHour = S.fromList $ map (Hour . fromIntegral) recurByHour
      rRuleByDay =
        S.fromList $
          map
            ( \case
                Left (i, wd) -> Specific i $ pickWeekDay wd
                Right wd -> Every $ pickWeekDay wd
            )
            recurByDay
      rRuleByMonthDay = S.fromList $ map MonthDay recurByMonthDay
      rRuleByYearDay = S.fromList $ map YearDay recurByYearDay
      rRuleByWeekNo = S.fromList $ map WeekNo recurByWeekNo
      rRuleByMonth = S.fromList $ mapMaybe monthNoToMonth recurByMonth -- Will ignore invalid values
      rRuleBySetPos = S.fromList $ map SetPos recurBySetPos
      rRuleWeekStart = pickWeekDay recurWkSt
   in RRule {..}

pickWeekDay :: ICal.Weekday -> DayOfWeek
pickWeekDay = \case
  ICal.Sunday -> Sunday
  ICal.Monday -> Monday
  ICal.Tuesday -> Tuesday
  ICal.Wednesday -> Wednesday
  ICal.Thursday -> Thursday
  ICal.Friday -> Friday
  ICal.Saturday -> Saturday
