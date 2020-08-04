{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.Recur where

import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as M
import Data.Time
import GHC.Generics (Generic)
import Smos.Calendar.Import.Event
import Smos.Calendar.Import.RecurringEvent
import Smos.Data

recurEvents :: TimeZone -> [RecurringEvents] -> [Event]
recurEvents tz = concatMap (recurRecurringEvents tz)

recurRecurringEvents :: TimeZone -> RecurringEvents -> [Event]
recurRecurringEvents tz RecurringEvents {..} =
  let ctx =
        RecurCtx
          { recurCtxTimeZone = tz,
            recurCtxTimeZones = recurringEventsTimeZones
          }
   in runReader (concat <$> mapM recurEvent recurringEvents) ctx

data RecurCtx
  = RecurCtx
      { recurCtxTimeZone :: TimeZone,
        recurCtxTimeZones :: Map TimeZoneId TimeZoneHistory
      }
  deriving (Show, Eq, Generic)

type R = Reader RecurCtx

recurEvent :: RecurringEvent -> R [Event]
recurEvent RecurringEvent {..} = do
  let eventSummary = recurringEventSummary
      eventDescription = recurringEventDescription
  eventStart <- mapM recurStart recurringEventStart
  eventEnd <- case recurringEventEnd of
    Nothing -> pure Nothing
    Just ced -> recurEndDuration eventStart ced
  pure [Event {..}]

recurStart :: CalTimestamp -> R Timestamp
recurStart = recurTimestamp

recurEndDuration :: Maybe Timestamp -> CalEndDuration -> R (Maybe Timestamp)
recurEndDuration mstart = \case
  CalTimestamp ts -> Just <$> recurTimestamp ts
  CalDuration ndt -> pure $ do
    start <- mstart
    let lt = timestampLocalTime start
    pure $ TimestampLocalTime $ addLocalTime (fromIntegral ndt) lt

recurTimestamp :: CalTimestamp -> R Timestamp
recurTimestamp = \case
  CalDate d -> pure $ TimestampDay d
  CalDateTime dt -> TimestampLocalTime <$> recurDateTime dt

recurDateTime :: CalDateTime -> R LocalTime
recurDateTime = \case
  Floating lt -> pure lt
  UTC lt -> pure $ utcToLocalTime utc lt
  Zoned lt tzid -> recurZonedTime lt tzid

recurZonedTime :: LocalTime -> TimeZoneId -> R LocalTime
recurZonedTime lt tzid = do
  RecurCtx {..} <- ask
  pure $ case M.lookup tzid recurCtxTimeZones of
    Nothing -> lt
    Just tzh -> recurZonedTimeWithHistory recurCtxTimeZone lt tzh

recurZonedTimeWithHistory :: TimeZone -> LocalTime -> TimeZoneHistory -> LocalTime
recurZonedTimeWithHistory tz lt TimeZoneHistory {..} =
  let utct = localTimeToUTC tz lt
      tz' =
        utcOffsetTimeZone $
          if lt < timeZoneHistoryStart
            then timeZoneHistoryOffsetFrom
            else timeZoneHistoryOffsetTo
   in utcToLocalTime tz' utct

utcOffsetTimeZone :: UTCOffset -> TimeZone
utcOffsetTimeZone (UTCOffset m) = minutesToTimeZone m
