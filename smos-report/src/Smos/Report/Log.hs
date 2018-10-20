{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.Log where

import GHC.Generics (Generic)

import Data.List
import qualified Data.Map as M
import Data.Map (Map)
import Data.Time

import Smos.Data

import Smos.Report.Path

type LogReport = [LogEntry]

data LogEntry = LogEntry
    { logEntryFilePath :: RootedPath
    , logEntryHeader :: Header
    , logEntryEvent :: LogEvent
    } deriving (Show, Eq, Generic)

data LogEvent = LogEvent
    { logEventTimestamp :: UTCTime
    , logEventType :: LogEventType
    } deriving (Show, Eq, Ord, Generic)

data LogEventType
    = StateChange (Maybe TodoState)
                  (Maybe TodoState)
    | ClockIn
    | ClockOut
    | TimestampEvent TimestampName
    deriving (Show, Eq, Ord, Generic)

makeLogReport :: TimeZone -> [(RootedPath, Entry)] -> [LogEntry]
makeLogReport tz =
    sortOn logEntryEvent . concatMap (uncurry $ makeLogEntries tz)

makeLogEntries :: TimeZone -> RootedPath -> Entry -> [LogEntry]
makeLogEntries tz rp e =
    map (\le ->
             LogEntry
                 { logEntryHeader = entryHeader e
                 , logEntryFilePath = rp
                 , logEntryEvent = le
                 }) $
    concat
        [ makeLogbookLogEntries e
        , makeStateHistoryLogEntries e
        , makeTimestampLogEntries tz e
        ]

makeLogbookLogEntries :: Entry -> [LogEvent]
makeLogbookLogEntries e = go (entryLogbook e)
  where
    go (LogOpen u es) =
        LogEvent {logEventTimestamp = u, logEventType = ClockIn} : goEs es
    go (LogClosed es) = goEs es
    goEs = concatMap goE
    goE LogbookEntry {..} =
        [ LogEvent
              {logEventTimestamp = logbookEntryStart, logEventType = ClockIn}
        , LogEvent
              {logEventTimestamp = logbookEntryEnd, logEventType = ClockOut}
        ]

makeStateHistoryLogEntries :: Entry -> [LogEvent]
makeStateHistoryLogEntries =
    map (uncurry go) . conseqs . unStateHistory . entryStateHistory
  where
    go :: StateHistoryEntry -> StateHistoryEntry -> LogEvent
    go old new =
        LogEvent
            { logEventTimestamp = stateHistoryEntryTimestamp new
            , logEventType =
                  StateChange
                      (stateHistoryEntryNewState old)
                      (stateHistoryEntryNewState new)
            }
    conseqs :: [a] -> [(a, a)]
    conseqs [] = []
    conseqs [_] = []
    conseqs (a:b:as) = (b, a) : conseqs (b : as)

makeTimestampLogEntries :: TimeZone -> Entry -> [LogEvent]
makeTimestampLogEntries tz = map (uncurry go) . M.toList . entryTimestamps
  where
    go :: TimestampName -> Timestamp -> LogEvent
    go tsn ts =
        LogEvent
            { logEventTimestamp =
                  localTimeToUTC tz $ LocalTime (timestampDay ts) midnight
            , logEventType = TimestampEvent tsn
            }
