{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.Resolve where

import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as M
import Data.Time
import GHC.Generics (Generic)
import Safe
import Smos.Calendar.Import.Event
import Smos.Calendar.Import.Static
import Smos.Calendar.Import.TimeZone
import Smos.Calendar.Import.UnresolvedEvent
import Smos.Calendar.Import.UnresolvedTimestamp
import Smos.Data

resolveEvents :: TimeZone -> [UnresolvedEvents] -> [Events]
resolveEvents tz = concatMap (resolveUnresolvedEvents tz)

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
    Nothing -> pure Nothing
    Just ced -> resolveEndDuration eventStart ced
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
resolveZonedTimeWithHistory tz lt (TimeZoneHistory rules) = case rules of
  [] -> lt -- Nothing we can do, and not allowed by the spec, but we can do this to not crash.
  [rule] -> resolveZonedTimeWithRule tz lt rule
  _ -> error "timezones with more than one rule not supported"

resolveZonedTimeWithRule :: TimeZone -> LocalTime -> TimeZoneHistoryRule -> LocalTime
resolveZonedTimeWithRule tz lt TimeZoneHistoryRule {..} =
  let utct = localTimeToUTC tz lt
      tz' =
        utcOffsetTimeZone $
          if lt < timeZoneHistoryRuleStart
            then timeZoneHistoryRuleOffsetFrom
            else timeZoneHistoryRuleOffsetTo
   in utcToLocalTime tz' utct

utcOffsetTimeZone :: UTCOffset -> TimeZone
utcOffsetTimeZone (UTCOffset m) = minutesToTimeZone m
