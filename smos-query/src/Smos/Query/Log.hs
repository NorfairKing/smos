{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Query.Log where

import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time

import Conduit
import qualified Data.Conduit.Combinators as C
import Rainbow

import Smos.Report.Filter
import Smos.Report.Log
import Smos.Report.Streaming
import Smos.Report.TimeBlock

import Smos.Query.Config
import Smos.Query.Formatting
import Smos.Query.OptParse.Types
import Smos.Query.Streaming

log :: LogSettings -> Q ()
log LogSettings {..} = do
  zt <- liftIO getZonedTime
  es <-
    sourceToList $
    streamSmosFiles logSetHideArchive .| parseSmosFiles .| printShouldPrint PrintWarning .|
    smosFileCursors .|
    C.filter (\(rp, fc) -> maybe True (\f -> filterPredicate f rp fc) logSetFilter) .|
    smosCursorCurrents
  liftIO $ putTableLn $ renderLogReport zt $ makeLogReport zt logSetPeriod logSetBlock es

renderLogReport :: ZonedTime -> LogReport -> Table
renderLogReport zt lrbs =
  formatAsTable $
  case lrbs of
    [] -> []
    [lrb] -> goEntries (blockEntries lrb)
    _ -> concatMap goEntriesWithTitle lrbs
  where
    goEntriesWithTitle Block {..} = [fore blue $ chunk blockTitle] : goEntries blockEntries
    goEntries es = map (renderLogEntry zt) (sortOn logEntryEvent es)

renderLogEntry :: ZonedTime -> LogEntry -> [Chunk Text]
renderLogEntry zt LogEntry {..} =
  let LogEvent {..} = logEntryEvent
   in [ rootedPathChunk logEntryFilePath
      , headerChunk logEntryHeader
      , chunk $
        T.pack $
        formatTime defaultTimeLocale "%F %X" $ utcToLocalTime (zonedTimeZone zt) logEventTimestamp
      ] <>
      logEventTypeChunk logEventType

logEventTypeChunk :: LogEventType -> [Chunk Text]
logEventTypeChunk typ =
  case typ of
    StateChange from to -> [mTodoStateChunk from, chunk " -> ", mTodoStateChunk to]
    ClockIn -> [chunk "clock in"]
    ClockOut -> [chunk "clock out"]
    TimestampEvent tsn -> [timestampNameChunk tsn]
