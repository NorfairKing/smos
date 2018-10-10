{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Agenda where

import GHC.Generics (Generic)

import qualified Data.Map as M
import Data.Maybe
import Data.Time

import Smos.Data

import Smos.Report.Agenda.Types
import Smos.Report.Path

data AgendaEntry = AgendaEntry
    { agendaEntryFilePath :: RootedPath
    , agendaEntryHeader :: Header
    , agendaEntryTodoState :: Maybe TodoState
    , agendaEntryTimestampName :: TimestampName
    , agendaEntryTimestamp :: Timestamp
    } deriving (Show, Eq, Generic)

isDone :: Maybe TodoState -> Bool
isDone (Just "DONE") = True
isDone (Just "CANCELLED") = True
isDone _ = False

makeAgendaEntry :: RootedPath -> Entry -> [AgendaEntry]
makeAgendaEntry rp e =
    flip mapMaybe (M.toList $ entryTimestamps e) $ \(tsn, ts) ->
        if isDone (entryState e)
            then Nothing
            else Just
                     AgendaEntry
                         { agendaEntryFilePath = rp
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
