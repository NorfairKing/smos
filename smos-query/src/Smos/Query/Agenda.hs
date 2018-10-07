{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Query.Agenda where

import Data.List
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time

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
        putTableLn $ renderAgendaReport $ sortOn agendaEntryTimestamp tups

renderAgendaReport :: [AgendaEntry] -> Table
renderAgendaReport = formatAsTable . map formatAgendaEntry

formatAgendaEntry :: AgendaEntry -> [Chunk Text]
formatAgendaEntry AgendaEntry {..} =
    [ chunk $ T.pack $ fromRelFile agendaEntryFilePath
    , chunk $ timestampNameText $ agendaEntryTimestampName
    , chunk $ timestampText agendaEntryTimestamp
    , headerChunk agendaEntryHeader
    ]
