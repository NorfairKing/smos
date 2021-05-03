{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Commands.Log
  ( smosQueryLog,
  )
where

import Conduit
import qualified Data.Text as T
import Smos.Query.Commands.Import
import Smos.Report.Log
import Smos.Report.TimeBlock

smosQueryLog :: LogSettings -> Q ()
smosQueryLog LogSettings {..} = do
  zt <- liftIO getZonedTime
  es <-
    sourceToList $
      streamSmosFiles logSetHideArchive
        .| streamParseSmosFiles
        .| smosFileCursors
        .| smosMFilter logSetFilter
        .| smosCursorCurrents
  let logReport = makeLogReport zt logSetPeriod logSetBlock es
  colourSettings <- asks envColourSettings
  outputChunks $ renderLogReport colourSettings zt logReport

renderLogReport :: ColourSettings -> ZonedTime -> LogReport -> [Chunk]
renderLogReport colourSettings zt lrbs =
  formatAsBicolourTable colourSettings $
    case lrbs of
      [] -> []
      [lrb] -> goEntries (blockEntries lrb)
      _ -> concatMap goEntriesWithTitle lrbs
  where
    goEntriesWithTitle Block {..} = [fore blue $ chunk blockTitle] : goEntries blockEntries
    goEntries es = map (renderLogEntry zt) (sortOn logEntryEvent es)

renderLogEntry :: ZonedTime -> LogEntry -> [Chunk]
renderLogEntry zt LogEntry {..} =
  let LogEvent {..} = logEntryEvent
   in [ pathChunk logEntryFilePath,
        headerChunk logEntryHeader,
        chunk $
          T.pack $
            formatTime defaultTimeLocale "%F %X" $
              utcToLocalTime (zonedTimeZone zt) logEventTimestamp
      ]
        <> logEventTypeChunk logEventType

logEventTypeChunk :: LogEventType -> [Chunk]
logEventTypeChunk typ =
  case typ of
    StateChange from to -> [mTodoStateChunk from, chunk " -> ", mTodoStateChunk to]
    ClockIn -> [chunk "clock in"]
    ClockOut -> [chunk "clock out"]
    TimestampEvent tsn -> [timestampNameChunk tsn]
