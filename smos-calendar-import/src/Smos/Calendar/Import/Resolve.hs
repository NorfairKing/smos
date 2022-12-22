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
import Smos.Calendar.Import.Event
import Smos.Calendar.Import.UnresolvedEvent
import Smos.Calendar.Import.UnresolvedTimestamp
import Smos.Data

resolveEvents :: LocalTime -> LocalTime -> TimeZone -> Set UnresolvedEvents -> Set Events
resolveEvents start end tz = S.unions . map (resolveUnresolvedEvents start end tz) . S.toList

resolveUnresolvedEvents :: LocalTime -> LocalTime -> TimeZone -> UnresolvedEvents -> Set Events
resolveUnresolvedEvents start end tz UnresolvedEvents {..} =
  let ctx =
        RecurCtx
          { resolveCtxTimeZone = tz,
            resolveCtxTimeZones = unresolvedEventsTimeZones
          }
   in S.fromList $ mapMaybe (filterEvents start end) $ runReader (mapM resolveEventGroup (S.toList unresolvedEventGroups)) ctx

filterEvents :: LocalTime -> LocalTime -> Events -> Maybe Events
filterEvents start end e@Events {..} =
  let s = S.filter (filterEvent start end) events
   in if S.null s then Nothing else Just $ e {events = s}

filterEvent :: LocalTime -> LocalTime -> Event -> Bool
filterEvent lo hi Event {..} = case (eventStart, eventEnd) of
  (Nothing, Nothing) -> True
  (Just start, Nothing) -> timestampLocalTime start <= hi
  (Nothing, Just end) -> lo <= timestampLocalTime end
  (Just start, Just end) ->
    timestampLocalTime start <= hi
      && lo <= timestampLocalTime end

data RecurCtx = RecurCtx
  { resolveCtxTimeZone :: TimeZone,
    resolveCtxTimeZones :: Map TimeZoneId TimeZoneHistory
  }
  deriving (Show, Eq, Generic)

type R = Reader RecurCtx

resolveEventGroup :: UnresolvedEventGroup -> R Events
resolveEventGroup UnresolvedEventGroup {..} = do
  let eventsStatic = unresolvedEventGroupStatic
  events <- S.fromList <$> mapM resolveEvent (S.toList unresolvedEvents)
  pure Events {..}

resolveEvent :: UnresolvedEvent -> R Event
resolveEvent UnresolvedEvent {..} = do
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
  UTC lt -> do
    tz <- asks resolveCtxTimeZone
    pure $ utcToLocalTime tz lt
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
    Just (ruleStart, (from, to)) -> resolveZonedTimeWithRule tz lt ruleStart from to

chooseRuleToApply :: LocalTime -> TimeZoneHistory -> Maybe (LocalTime, (UTCOffset, UTCOffset))
chooseRuleToApply lt (TimeZoneHistory rules) =
  let m = M.unions $ map (ruleRecurrences lt) rules
   in M.lookupLE lt m <|> M.lookupGE lt m

ruleRecurrences :: LocalTime -> TimeZoneHistoryRule -> Map LocalTime (UTCOffset, UTCOffset)
ruleRecurrences limit TimeZoneHistoryRule {..} =
  -- Always have the start
  let start = M.singleton timeZoneHistoryRuleStart (timeZoneHistoryRuleOffsetFrom, timeZoneHistoryRuleOffsetTo)
      rRuleSet = rruleSetDateTimeOccurrencesUntil timeZoneHistoryRuleStart timeZoneHistoryRuleRRules limit
      toUTCMap = M.fromSet (const (timeZoneHistoryRuleOffsetFrom, timeZoneHistoryRuleOffsetTo))
      onlyLocalTime = \case
        CalRTimestamp (CalDateTime (Floating lt)) -> Just lt
        _ -> Nothing
      rDateSet = S.fromList $ mapMaybe onlyLocalTime $ S.toList timeZoneHistoryRuleRDates
   in M.union start $ toUTCMap $ S.union rRuleSet rDateSet

resolveZonedTimeWithRule :: TimeZone -> LocalTime -> LocalTime -> UTCOffset -> UTCOffset -> LocalTime
resolveZonedTimeWithRule tz lt start from to =
  let tz' =
        utcOffsetTimeZone $
          if lt < start
            then from
            else to
      utct = localTimeToUTC tz' lt -- From the local time according to the TimeZoneHistoryRule to UTC
   in utcToLocalTime tz utct -- From UTC to the local time that we want

utcOffsetTimeZone :: UTCOffset -> TimeZone
utcOffsetTimeZone (UTCOffset m) = minutesToTimeZone m
