{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.Log where

import Data.List
import qualified Data.Map as M
import Data.Text (Text)
import Data.Time
import Data.Time.Zones
import Data.Validity
import Data.Validity.Path ()
import GHC.Generics (Generic)
import Path
import Smos.Data
import Smos.Report.Period
import Smos.Report.TimeBlock

type LogReport = [LogTableBlock Text]

type LogTableBlock a = Block a LogEntry

data LogEntry = LogEntry
  { logEntryFilePath :: Path Rel File,
    logEntryHeader :: Header,
    logEntryEvent :: LogEvent
  }
  deriving (Show, Eq, Generic)

instance Validity LogEntry

data LogEvent = LogEvent
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

makeLogReport :: TZ -> Day -> Period -> TimeBlock -> [(Path Rel File, Entry)] -> LogReport
makeLogReport zone today period timeBlock =
  let interval = periodInterval today period
   in divideIntoBlocks (logEntryDay zone) timeBlock
        . filter (filterIntervalDay interval . logEntryDay zone)
        . sortOn logEntryEvent
        . concatMap (uncurry $ makeLogEntries zone)

logEntryDay :: TZ -> LogEntry -> Day
logEntryDay tz = localDay . utcToLocalTimeTZ tz . logEventTimestamp . logEntryEvent

makeLogEntries :: TZ -> Path Rel File -> Entry -> [LogEntry]
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

makeTimestampLogEntries :: TZ -> Entry -> [LogEvent]
makeTimestampLogEntries tz = map (uncurry go) . M.toList . entryTimestamps
  where
    go :: TimestampName -> Timestamp -> LogEvent
    go tsn ts =
      LogEvent
        { logEventTimestamp = localTimeToUTCTZ tz $ timestampLocalTime ts,
          logEventType = TimestampEvent tsn
        }
