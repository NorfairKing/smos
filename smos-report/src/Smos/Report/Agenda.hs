{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Agenda where

import GHC.Generics (Generic)

import qualified Data.Map as M
import Data.Maybe
import Data.Time

import Path

import Smos.Data

import Smos.Report.Agenda.Types

data AgendaEntry = AgendaEntry
    { agendaEntryFilePath :: Path Rel File
    , agendaEntryHeader :: Header
    , agendaEntryTodoState :: Maybe TodoState
    , agendaEntryTimestampName :: TimestampName
    , agendaEntryTimestamp :: Timestamp
    } deriving (Show, Eq, Generic)

isDone :: Maybe TodoState -> Bool
isDone (Just "DONE") = True
isDone (Just "CANCELLED") = True
isDone _ = False

makeAgendaEntry :: Path Rel File -> Entry -> [AgendaEntry]
makeAgendaEntry rf e =
    flip mapMaybe (M.toList $ entryTimestamps e) $ \(tsn, ts) ->
        if isDone (entryState e)
            then Nothing
            else Just
                     AgendaEntry
                         { agendaEntryFilePath = rf
                         , agendaEntryHeader = entryHeader e
                         , agendaEntryTodoState = entryState e
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
