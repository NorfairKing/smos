{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.Log where

import Data.List
import qualified Data.Map as M
import Data.Text (Text)
import Data.Time
import Data.Validity
import GHC.Generics (Generic)
import Smos.Data
import Smos.Report.Path
import Smos.Report.Period
import Smos.Report.TimeBlock

type LogReport = [LogTableBlock Text]

type LogTableBlock a = Block a LogEntry

data LogEntry
  = LogEntry
      { logEntryFilePath :: RootedPath,
        logEntryHeader :: Header,
        logEntryEvent :: LogEvent
      }
  deriving (Show, Eq, Generic)

instance Validity LogEntry

data LogEvent
  = LogEvent
      { logEventTimestamp :: UTCTime,
        logEventType :: LogEventType
      }
  deriving (Show, Eq, Ord, Generic)

instance Validity LogEvent

-- The order of these constructors matters because ClockOut should happen before ClockIn
data LogEventType
  = StateChange (Maybe TodoState) (Maybe TodoState)
  | ClockOut
  | ClockIn
  | TimestampEvent TimestampName
  deriving (Show, Eq, Ord, Generic)

instance Validity LogEventType

makeLogReport :: ZonedTime -> Period -> TimeBlock -> [(RootedPath, Entry)] -> LogReport
makeLogReport zt pe tb =
  divideIntoBlocks (logEntryDay $ zonedTimeZone zt) tb
    . filter (filterPeriod zt pe . logEventTimestamp . logEntryEvent)
    . sortOn logEntryEvent
    . concatMap (uncurry $ makeLogEntries $ zonedTimeZone zt)

logEntryDay :: TimeZone -> LogEntry -> Day
logEntryDay tz = localDay . utcToLocalTime tz . logEventTimestamp . logEntryEvent

makeLogEntries :: TimeZone -> RootedPath -> Entry -> [LogEntry]
makeLogEntries tz rp e =
  map (\le -> LogEntry {logEntryHeader = entryHeader e, logEntryFilePath = rp, logEntryEvent = le}) $
    concat [makeLogbookLogEntries e, makeStateHistoryLogEntries e, makeTimestampLogEntries tz e]

makeLogbookLogEntries :: Entry -> [LogEvent]
makeLogbookLogEntries e = go (entryLogbook e)
  where
    go (LogOpen u es) = LogEvent {logEventTimestamp = u, logEventType = ClockIn} : goEs es
    go (LogClosed es) = goEs es
    goEs = concatMap goE
    goE LogbookEntry {..} =
      [ LogEvent {logEventTimestamp = logbookEntryStart, logEventType = ClockIn},
        LogEvent {logEventTimestamp = logbookEntryEnd, logEventType = ClockOut}
      ]

makeStateHistoryLogEntries :: Entry -> [LogEvent]
makeStateHistoryLogEntries = map (uncurry go) . conseqs . unStateHistory . entryStateHistory
  where
    go :: Maybe StateHistoryEntry -> StateHistoryEntry -> LogEvent
    go mold new =
      LogEvent
        { logEventTimestamp = stateHistoryEntryTimestamp new,
          logEventType = StateChange (mold >>= stateHistoryEntryNewState) (stateHistoryEntryNewState new)
        }
    conseqs :: [a] -> [(Maybe a, a)]
    conseqs [] = []
    conseqs [a] = [(Nothing, a)]
    conseqs (a : b : as) = (Just b, a) : conseqs (b : as)

makeTimestampLogEntries :: TimeZone -> Entry -> [LogEvent]
makeTimestampLogEntries tz = map (uncurry go) . M.toList . entryTimestamps
  where
    go :: TimestampName -> Timestamp -> LogEvent
    go tsn ts =
      LogEvent
        { logEventTimestamp = localTimeToUTC tz $ timestampLocalTime ts,
          logEventType = TimestampEvent tsn
        }
