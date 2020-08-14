{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.Resolve where

import Control.Applicative
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Data.Time
import GHC.Generics (Generic)
import Smos.Calendar.Import.Event
import Smos.Calendar.Import.RecurrenceRule
import Smos.Calendar.Import.TimeZone
import Smos.Calendar.Import.UnresolvedEvent
import Smos.Calendar.Import.UnresolvedTimestamp
import Smos.Data

resolveEvents :: LocalTime -> LocalTime -> TimeZone -> [UnresolvedEvents] -> [Events]
resolveEvents start end tz = mapMaybe (filterEvents start end) . concatMap (resolveUnresolvedEvents tz)

filterEvents :: LocalTime -> LocalTime -> Events -> Maybe Events
filterEvents start end e@Events {..} = case filter (filterEvent start end) events of
  [] -> Nothing
  es -> Just $ e {events = es}

filterEvent :: LocalTime -> LocalTime -> Event -> Bool
filterEvent lo hi Event {..} = case (eventStart, eventEnd) of
  (Nothing, Nothing) -> True
  (Just start, Nothing) -> timestampLocalTime start <= hi
  (Nothing, Just end) -> lo <= timestampLocalTime end
  (Just start, Just end) ->
    timestampLocalTime start <= hi
      && lo <= timestampLocalTime end

resolveUnresolvedEvents :: TimeZone -> UnresolvedEvents -> [Events]
resolveUnresolvedEvents tz UnresolvedEvents {..} =
  let ctx =
        RecurCtx
          { resolveCtxTimeZone = tz,
            resolveCtxTimeZones = unresolvedEventsTimeZones
          }
   in runReader (mapM resolveEventGroup unresolvedEventGroups) ctx

data RecurCtx
  = RecurCtx
      { resolveCtxTimeZone :: TimeZone,
        resolveCtxTimeZones :: Map TimeZoneId TimeZoneHistory
      }
  deriving (Show, Eq, Generic)

type R = Reader RecurCtx

resolveEventGroup :: UnresolvedEventGroup -> R Events
resolveEventGroup UnresolvedEventGroup {..} = do
  let eventsTitle = unresolvedEventGroupTitle
  events <- mapM resolveEvent unresolvedEvents
  pure Events {..}

resolveEvent :: UnresolvedEvent -> R Event
resolveEvent UnresolvedEvent {..} = do
  let eventStatic = unresolvedEventStatic
  eventStart <- mapM resolveStart unresolvedEventStart
  eventEnd <- case unresolvedEventEnd of
    Just ced -> resolveEndDuration eventStart ced
    -- Use the event start so we definitely have an endpoint. This is the way google calendar does it.
    -- This is important because otherwise very old events without an end time are always imported.
    Nothing -> pure eventStart
  pure Event {..}

resolveStart :: CalTimestamp -> R Timestamp
resolveStart = resolveTimestamp

resolveEndDuration :: Maybe Timestamp -> CalEndDuration -> R (Maybe Timestamp)
resolveEndDuration mstart = \case
  CalTimestamp ts -> Just <$> resolveTimestamp ts
  CalDuration ndt -> pure $ do
    start <- mstart
    let lt = timestampLocalTime start
    pure $ TimestampLocalTime $ addLocalTime (fromIntegral ndt) lt

resolveTimestamp :: CalTimestamp -> R Timestamp
resolveTimestamp = \case
  CalDate d -> pure $ TimestampDay d
  CalDateTime dt -> TimestampLocalTime <$> resolveDateTime dt

resolveDateTime :: CalDateTime -> R LocalTime
resolveDateTime = \case
  Floating lt -> pure lt
  UTC lt -> pure $ utcToLocalTime utc lt
  Zoned lt tzid -> resolveZonedTime lt tzid

resolveZonedTime :: LocalTime -> TimeZoneId -> R LocalTime
resolveZonedTime lt tzid = do
  RecurCtx {..} <- ask
  pure $ case M.lookup tzid resolveCtxTimeZones of
    Nothing -> lt
    Just tzh -> resolveZonedTimeWithHistory resolveCtxTimeZone lt tzh

resolveZonedTimeWithHistory :: TimeZone -> LocalTime -> TimeZoneHistory -> LocalTime
resolveZonedTimeWithHistory tz lt tzh =
  case chooseRuleToApply lt tzh of
    Nothing -> lt -- Nothing we can do, and not allowed by the spec, but we can do this to not crash.
    Just rule -> resolveZonedTimeWithRule tz lt rule

chooseRuleToApply :: LocalTime -> TimeZoneHistory -> Maybe TimeZoneHistoryRule
chooseRuleToApply lt (TimeZoneHistory rules) =
  let m = M.unions $ map (ruleRecurrences lt) rules
   in snd <$> (M.lookupLE lt m <|> M.lookupGE lt m)

-- TODO: have an intermediate type for a TimeZoneHistoryRule without recurrence

ruleRecurrences :: LocalTime -> TimeZoneHistoryRule -> Map LocalTime TimeZoneHistoryRule
ruleRecurrences limit tzh@TimeZoneHistoryRule {..} =
  let s = rruleSetDateTimeOccurrencesUntil timeZoneHistoryRuleStart timeZoneHistoryRuleRRules limit
      s' = if S.null s then S.singleton timeZoneHistoryRuleStart else s -- If there is no recurrence, just use the rule by itself
   in M.fromList $ map (\lt -> (lt, tzh {timeZoneHistoryRuleStart = lt})) $ S.toList s'

resolveZonedTimeWithRule :: TimeZone -> LocalTime -> TimeZoneHistoryRule -> LocalTime
resolveZonedTimeWithRule tz lt TimeZoneHistoryRule {..} =
  let tz' =
        utcOffsetTimeZone $
          if lt < timeZoneHistoryRuleStart
            then timeZoneHistoryRuleOffsetFrom
            else timeZoneHistoryRuleOffsetTo
      utct = localTimeToUTC tz' lt -- From the local time according to the TimeZoneHistoryRule to UTC
   in utcToLocalTime tz utct -- From UTC to the local time that we want

utcOffsetTimeZone :: UTCOffset -> TimeZone
utcOffsetTimeZone (UTCOffset m) = minutesToTimeZone m
