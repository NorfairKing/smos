{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.Resolve where

import Control.Applicative
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Time
import GHC.Generics (Generic)
import qualified ICal.Component.TimeZone as ICal
import qualified ICal.Conformance as ICal
import qualified ICal.Parameter as ICal
import qualified ICal.Property as ICal
import qualified ICal.PropertyType as ICal
import qualified ICal.Recurrence as ICal
import qualified ICal.Recurrence.Class as ICal
import Smos.Calendar.Import.Event
import Smos.Calendar.Import.UnresolvedEvent
import Smos.Data

resolveEvents :: Day -> Day -> TimeZone -> Set UnresolvedEvents -> Set Events
resolveEvents start end tz = S.unions . map (resolveUnresolvedEvents start end tz) . S.toList

resolveUnresolvedEvents :: Day -> Day -> TimeZone -> UnresolvedEvents -> Set Events
resolveUnresolvedEvents start end tz UnresolvedEvents {..} =
  let ctx =
        RecurCtx
          { resolveCtxTimeZone = tz,
            resolveCtxTimeZones = unresolvedEventsTimeZones
          }
   in S.fromList $
        flip mapMaybe (S.toList unresolvedEventGroups) $ \eg ->
          let errOrEvents = ICal.runConform $ runReaderT (resolveEventGroup eg) ctx
           in case errOrEvents of
                Left _ -> Nothing
                Right (events, _) -> filterEvents start end events

filterEvents :: Day -> Day -> Events -> Maybe Events
filterEvents start end e@Events {..} =
  let s = S.filter (filterEvent start end) events
   in if S.null s then Nothing else Just $ e {events = s}

filterEvent :: Day -> Day -> Event -> Bool
filterEvent lo hi Event {..} = case (eventStart, eventEnd) of
  (Nothing, Nothing) -> True
  (Just start, Nothing) -> timestampDay start <= hi
  (Nothing, Just end) -> lo <= timestampDay end
  (Just start, Just end) ->
    timestampDay start <= hi
      && lo <= timestampDay end

data RecurCtx = RecurCtx
  { resolveCtxTimeZone :: TimeZone,
    resolveCtxTimeZones :: Map ICal.TZIDParam ICal.TimeZone
  }
  deriving (Show, Eq, Generic)

type R a = ReaderT RecurCtx ICal.Resolv a

resolveEventGroup :: UnresolvedEventGroup -> R Events
resolveEventGroup UnresolvedEventGroup {..} = do
  let eventsStatic = unresolvedEventGroupStatic
  events <- S.fromList <$> mapM resolveEvent (S.toList unresolvedEvents)
  pure Events {..}

resolveEvent :: ICal.EventOccurrence -> R Event
resolveEvent ICal.EventOccurrence {..} = do
  eventStart <- mapM resolveStart eventOccurrenceStart
  eventEnd <- case eventOccurrenceEndOrDuration of
    Just ced -> resolveEndDuration eventStart ced
    -- Use the event start so we definitely have an endpoint. This is the way google calendar does it.
    -- This is important because otherwise very old events without an end time are always imported.
    Nothing -> pure eventStart
  pure Event {..}

resolveStart :: ICal.DateTimeStart -> R Timestamp
resolveStart = \case
  ICal.DateTimeStartDate date -> pure $ TimestampDay $ resolveDate date
  ICal.DateTimeStartDateTime dateTime -> TimestampLocalTime <$> resolveDateTime dateTime

resolveEndDuration :: Maybe Timestamp -> Either ICal.DateTimeEnd ICal.Duration -> R (Maybe Timestamp)
resolveEndDuration mts = \case
  Left end -> Just <$> resolveEnd end
  Right duration -> pure $ case mts of
    Nothing -> Nothing -- Start timestamp but no end timestamp: Nothing we can do.
    Just ts ->
      Just $
        let ndt = ICal.durationNominalDiffTime duration
         in case ts of
              TimestampDay d ->
                let (diff, _) = timeToDaysAndTimeOfDay ndt
                 in TimestampDay $ addDays diff d
              TimestampLocalTime lt -> TimestampLocalTime $ addLocalTime ndt lt

resolveEnd :: ICal.DateTimeEnd -> R Timestamp
resolveEnd = \case
  ICal.DateTimeEndDate date -> pure $ TimestampDay $ resolveDate date
  ICal.DateTimeEndDateTime dateTime -> TimestampLocalTime <$> resolveDateTime dateTime

resolveDate :: ICal.Date -> Day
resolveDate = ICal.unDate

resolveDateTime :: ICal.DateTime -> R LocalTime
resolveDateTime = \case
  ICal.DateTimeFloating lt -> pure lt
  ICal.DateTimeUTC utcTime -> resolveUTCTime utcTime
  ICal.DateTimeZoned tzid lt -> resolveLocalTime tzid lt

resolveLocalTime :: ICal.TZIDParam -> LocalTime -> R LocalTime
resolveLocalTime tzid localTime = do
  zones <- asks resolveCtxTimeZones
  case M.lookup tzid zones of
    Nothing -> lift $ ICal.unfixableError $ ICal.TimeZoneNotFound tzid
    Just zone -> do
      mUTCTime <- lift $ ICal.resolveLocalTime zone localTime
      case mUTCTime of
        Nothing -> lift $ ICal.unfixableError $ ICal.FailedToResolveLocalTime zone localTime
        Just utcTime -> resolveUTCTime utcTime

resolveUTCTime :: UTCTime -> R LocalTime
resolveUTCTime utcTime = do
  myZone <- asks resolveCtxTimeZone
  pure $ utcToLocalTime myZone utcTime

-- resolveEndDuration mstart = \case
--   CalTimestamp ts -> Just <$> resolveTimestamp ts
--   CalDuration ndt -> pure $ do
--     start <- mstart
--     let lt = timestampLocalTime start
--     pure $ TimestampLocalTime $ addLocalTime (fromIntegral ndt) lt
--
-- resolveTimestamp :: CalTimestamp -> R Timestamp
-- resolveTimestamp = \case
--   CalDate d -> pure $ TimestampDay d
--   CalDateTime dt -> TimestampLocalTime <$> resolveDateTime dt
--
-- resolveDateTime :: CalDateTime -> R LocalTime
-- resolveDateTime = \case
--   Floating lt -> pure lt
--   UTC lt -> do
--     tz <- asks resolveCtxTimeZone
--     pure $ utcToLocalTime tz lt
--   Zoned lt tzid -> resolveZonedTime lt tzid
--
-- resolveZonedTime :: LocalTime -> ICal.TZID -> R LocalTime
-- resolveZonedTime lt tzid = do
--   RecurCtx {..} <- ask
--   pure $ case M.lookup tzid resolveCtxTimeZones of
--     Nothing -> lt
--     Just tzh -> resolveZonedTimeWithHistory resolveCtxTimeZone lt tzh

resolveZonedTimeWithHistory :: TimeZone -> LocalTime -> ICal.TimeZone -> LocalTime
resolveZonedTimeWithHistory tz lt tzh = undefined -- TODO: In ICal
-- case chooseRuleToApply lt tzh of
--   Nothing -> lt -- Nothing we can do, and not allowed by the spec, but we can do this to not crash.
--   Just (ruleStart, (from, to)) -> resolveZonedTimeWithRule tz lt ruleStart from to

-- chooseRuleToApply :: LocalTime -> ICal.TimeZone -> Maybe (LocalTime, (UTCOffset, UTCOffset))
-- chooseRuleToApply lt (TimeZoneHistory rules) =
--   let m = M.unions $ map (ruleRecurrences lt) rules
--    in M.lookupLE lt m <|> M.lookupGE lt m

-- ruleRecurrences :: LocalTime -> TimeZoneHistoryRule -> Map LocalTime (UTCOffset, UTCOffset)
-- ruleRecurrences limit TimeZoneHistoryRule {..} =
--   -- Always have the start
--   let start = M.singleton timeZoneHistoryRuleStart (timeZoneHistoryRuleOffsetFrom, timeZoneHistoryRuleOffsetTo)
--       rRuleSet = rruleSetDateTimeOccurrencesUntil timeZoneHistoryRuleStart timeZoneHistoryRuleRRules limit
--       toUTCMap = M.fromSet (const (timeZoneHistoryRuleOffsetFrom, timeZoneHistoryRuleOffsetTo))
--       onlyLocalTime = \case
--         CalRTimestamp (CalDateTime (Floating lt)) -> Just lt
--         _ -> Nothing
--       rDateSet = S.fromList $ mapMaybe onlyLocalTime $ S.toList timeZoneHistoryRuleRDates
--    in M.union start $ toUTCMap $ S.union rRuleSet rDateSet
--
-- resolveZonedTimeWithRule :: TimeZone -> LocalTime -> LocalTime -> UTCOffset -> UTCOffset -> LocalTime
-- resolveZonedTimeWithRule tz lt start from to =
--   let tz' =
--         utcOffsetTimeZone $
--           if lt < start
--             then from
--             else to
--       utct = localTimeToUTC tz' lt -- From the local time according to the TimeZoneHistoryRule to UTC
--    in utcToLocalTime tz utct -- From UTC to the local time that we want
--
-- utcOffsetTimeZone :: UTCOffset -> TimeZone
-- utcOffsetTimeZone (UTCOffset m) = minutesToTimeZone m
