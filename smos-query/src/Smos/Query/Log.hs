{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Query.Log where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time

import qualified Data.Map as M
import Data.Map (Map)

import Conduit
import qualified Data.Conduit.Combinators as C
import Rainbow

import Smos.Data

import Smos.Report.Log
import Smos.Report.Query
import Smos.Report.Streaming

import Smos.Query.Config
import Smos.Query.Formatting
import Smos.Query.OptParse.Types

log :: LogSettings -> Q ()
log LogSettings {..} = do
    wd <- askWorkDir
    liftIO $ do
        zt <- getZonedTime
        es <-
            sourceToList $
            sourceFilesInNonHiddenDirsRecursively wd .| filterSmosFiles .|
            parseSmosFiles .|
            printShouldPrint PrintWarning .|
            smosFileCursors .|
            C.filter (maybe (const True) filterPredicate logSetFilter . snd) .|
            smosCursorCurrents
        putTableLn $ renderLogReport zt $ makeLogReport (zonedTimeZone zt) es

renderLogReport :: ZonedTime -> [LogEntry] -> Table
renderLogReport zt = formatAsTable . map (renderLogEntry zt)

renderLogEntry :: ZonedTime -> LogEntry -> [Chunk Text]
renderLogEntry zt LogEntry {..} =
    let LogEvent {..} = logEntryEvent
     in [ rootedPathChunk logEntryFilePath
        , headerChunk logEntryHeader
        , chunk $
          T.pack $
          formatTime defaultTimeLocale "%F %X" $
          utcToLocalTime (zonedTimeZone zt) logEventTimestamp
        ] <>
        logEventTypeChunk logEventType

logEventTypeChunk :: LogEventType -> [Chunk Text]
logEventTypeChunk typ =
    case typ of
        StateChange from to ->
            [mTodoStateChunk from, chunk " -> ", mTodoStateChunk to]
        ClockIn -> [chunk "clock in"]
        ClockOut -> [chunk "clock out"]
        TimestampEvent tsn -> [timestampNameChunk tsn]
