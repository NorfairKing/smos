{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Agenda where

import GHC.Generics (Generic)

import Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T

import Text.Show.Pretty

import Conduit
import qualified Data.Conduit.Combinators as C
import Path

import Smos.Data

import Smos.Report.Formatting
import Smos.Report.OptParse
import Smos.Report.Streaming

import Smos.Report.Agenda.Types

agenda :: Settings -> IO ()
agenda Settings {..} = do
    tups <-
        sourceToList $
        sourceFilesInNonHiddenDirsRecursively setWorkDir .| filterSmosFiles .|
        parseSmosFiles setWorkDir .|
        printShouldPrint setShouldPrint .|
        smosFileEntries .|
        C.concatMap (uncurry makeAgendaEntry)
    T.putStr $ renderAgendaReport $ sortOn agendaEntryTimestamp tups

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

renderAgendaReport :: [AgendaEntry] -> Text
renderAgendaReport = T.pack . formatAsTable . map formatAgendaEntry

formatAgendaEntry :: AgendaEntry -> [String]
formatAgendaEntry AgendaEntry {..} =
    [ fromRelFile agendaEntryFilePath
    , T.unpack $ timestampNameText $ agendaEntryTimestampName
    , timestampString agendaEntryTimestamp
    , T.unpack $ headerText agendaEntryHeader
    ]
