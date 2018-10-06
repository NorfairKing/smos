{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Agenda where

import GHC.Generics (Generic)

import Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time

import Conduit
import qualified Data.Conduit.Combinators as C
import Path

import Smos.Data

import Smos.Report.Formatting
import Smos.Report.OptParse
import Smos.Report.Streaming

agenda :: AgendaSettings -> Settings -> IO ()
agenda AgendaSettings {..} Settings {..} = do
    now <- getZonedTime
    tups <-
        sourceToList $
        sourceFilesInNonHiddenDirsRecursively setWorkDir .| filterSmosFiles .|
        parseSmosFiles setWorkDir .|
        printShouldPrint setShouldPrint .|
        smosFileEntries .|
        C.concatMap (uncurry makeAgendaEntry) .|
        C.filter (fitsHistoricity now agendaSetHistoricity)
    putTableLn $ renderAgendaReport $ sortOn agendaEntryTimestamp tups

data AgendaEntry = AgendaEntry
    { agendaEntryFilePath :: Path Rel File
    , agendaEntryHeader :: Header
    , agendaEntryTimestampName :: TimestampName
    , agendaEntryTimestamp :: Timestamp
    } deriving (Show, Eq, Generic)

makeAgendaEntry :: Path Rel File -> Entry -> [AgendaEntry]
makeAgendaEntry rf e =
    flip map (M.toList $ entryTimestamps e) $ \(tsn, ts) ->
        AgendaEntry
            { agendaEntryFilePath = rf
            , agendaEntryHeader = entryHeader e
            , agendaEntryTimestampName = tsn
            , agendaEntryTimestamp = ts
            }

fitsHistoricity :: ZonedTime -> AgendaHistoricity -> AgendaEntry -> Bool
fitsHistoricity zt ah ae =
    case ah of
        HistoricalAgenda -> True
        FutureAgenda ->
            LocalTime (timestampDay (agendaEntryTimestamp ae)) midnight >=
            zonedTimeToLocalTime zt

renderAgendaReport :: [AgendaEntry] -> Table
renderAgendaReport = formatAsTable . map formatAgendaEntry

formatAgendaEntry :: AgendaEntry -> [Text]
formatAgendaEntry AgendaEntry {..} =
    [ T.pack $ fromRelFile agendaEntryFilePath
    , timestampNameText $ agendaEntryTimestampName
    , timestampText agendaEntryTimestamp
    , headerText agendaEntryHeader
    ]
