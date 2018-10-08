{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Query.Agenda where

import Data.List
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time
import Text.Printf

import Conduit
import qualified Data.Conduit.Combinators as C
import Path
import Rainbow

import Smos.Data

import Smos.Report.Agenda
import Smos.Report.Streaming

import Smos.Query.Config
import Smos.Query.Formatting
import Smos.Query.OptParse.Types

agenda :: AgendaSettings -> Q ()
agenda AgendaSettings {..} = do
    wd <- askWorkDir
    liftIO $ do
        now <- getZonedTime
        tups <-
            sourceToList $
            sourceFilesInNonHiddenDirsRecursively wd .| filterSmosFiles .|
            parseSmosFiles wd .|
            printShouldPrint PrintWarning .|
            smosFileEntries .|
            C.concatMap (uncurry makeAgendaEntry) .|
            C.filter (fitsHistoricity now agendaSetHistoricity)
        putTableLn $ renderAgendaReport now tups

renderAgendaReport :: ZonedTime -> [AgendaEntry] -> Table
renderAgendaReport now =
    formatAsTable . map (formatAgendaEntry now) . sortOn agendaEntryTimestamp

formatAgendaEntry :: ZonedTime -> AgendaEntry -> [Chunk Text]
formatAgendaEntry now AgendaEntry {..} =
    let d =
            diffDays
                (timestampDay agendaEntryTimestamp)
                (localDay $ zonedTimeToLocalTime now)
        func =
            if | d <= 0 && agendaEntryTimestampName == "DEADLINE" -> fore red
               | d <= 10 && agendaEntryTimestampName == "DEADLINE" ->
                   fore yellow
               | d < 0 && agendaEntryTimestampName == "SCHEDULED" -> fore red
               | d == 0 && agendaEntryTimestampName == "SCHEDULED" -> fore green
               | otherwise -> id
     in [ func $ chunk $ T.pack $ fromRelFile agendaEntryFilePath
        , func $ chunk $ timestampText agendaEntryTimestamp
        , func $ chunk $ T.pack $ printf "%+3dd" d
        , timestampNameChunk $ agendaEntryTimestampName
        , maybe (chunk "") todoStateChunk agendaEntryTodoState
        , headerChunk agendaEntryHeader
        ]
