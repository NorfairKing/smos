{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Commands.Log
  ( smosQueryLog,
  )
where

import Conduit
import qualified Data.Text as T
import Data.Time.Zones
import Smos.Query.Commands.Import
import Smos.Report.Log
import Smos.Report.TimeBlock

smosQueryLog :: LogSettings -> Q ()
smosQueryLog LogSettings {..} = do
  zone <- liftIO loadLocalTZ
  now <- liftIO getCurrentTime
  let today = localDay (utcToLocalTimeTZ zone now)
  es <-
    sourceToList $
      streamSmosFiles logSetHideArchive
        .| streamParseSmosFiles
        .| smosFileCursors
        .| mFilterConduit logSetFilter
        .| smosCursorCurrents
  let logReport = makeLogReport zone today logSetPeriod logSetBlock es
  colourSettings <- asks envColourSettings
  outputChunks $ renderLogReport colourSettings zone logReport

renderLogReport :: ColourSettings -> TZ -> LogReport -> [Chunk]
renderLogReport colourSettings zone lrbs =
  formatAsBicolourTable colourSettings $
    case lrbs of
      [] -> []
      [lrb] -> goEntries (blockEntries lrb)
      _ -> concatMap goEntriesWithTitle lrbs
  where
    goEntriesWithTitle Block {..} = [fore blue $ chunk blockTitle] : goEntries blockEntries
    goEntries es = map (renderLogEntry zone) (sortOn logEntryEvent es)

renderLogEntry :: TZ -> LogEntry -> [Chunk]
renderLogEntry zone LogEntry {..} =
  let LogEvent {..} = logEntryEvent
   in [ pathChunk logEntryFilePath,
        headerChunk logEntryHeader,
        chunk $
          T.pack $
            formatTime defaultTimeLocale "%F %X" $
              utcToLocalTimeTZ zone logEventTimestamp
      ]
        <> logEventTypeChunk logEventType

logEventTypeChunk :: LogEventType -> [Chunk]
logEventTypeChunk typ =
  case typ of
    StateChange from to -> [mTodoStateChunk from, chunk " -> ", mTodoStateChunk to]
    ClockIn -> [chunk "clock in"]
    ClockOut -> [chunk "clock out"]
    TimestampEvent tsn -> [timestampNameChunk tsn]
