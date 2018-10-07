{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Agenda where

import GHC.Generics (Generic)

import qualified Data.Map as M
import Data.Time

import Path

import Smos.Data

import Smos.Report.Agenda.Types

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
