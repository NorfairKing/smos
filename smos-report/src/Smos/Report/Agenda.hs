{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Agenda where

import GHC.Generics (Generic)

import Data.Function
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time
import Data.Validity

import Smos.Data

import Smos.Report.Agenda.Types
import Smos.Report.Path
import Smos.Report.TimeBlock

data AgendaEntry = AgendaEntry
    { agendaEntryFilePath :: RootedPath
    , agendaEntryHeader :: Header
    , agendaEntryTodoState :: Maybe TodoState
    , agendaEntryTimestampName :: TimestampName
    , agendaEntryTimestamp :: Timestamp
    } deriving (Show, Eq, Generic)

instance Validity AgendaEntry

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

data AgendaTableBlock a = AgendaTableBlock
    { agendaTableBlockTitle :: a
    , agendaTableBlockEntries :: [AgendaEntry]
    } deriving (Show, Eq, Generic, Functor)

instance Validity a => Validity (AgendaTableBlock a)

divideIntoBlocks :: TimeBlock -> [AgendaEntry] -> [AgendaTableBlock Text]
divideIntoBlocks tb aes =
    case tb of
        OneBlock ->
            [ AgendaTableBlock
                  { agendaTableBlockTitle = "All Time"
                  , agendaTableBlockEntries = aes
                  }
            ]
        DayBlock ->
            map (fmap (T.pack . show)) $
            sortOn agendaTableBlockTitle $
            combineBlocksByName $ map turnIntoSingletonBlock aes

turnIntoSingletonBlock :: AgendaEntry -> AgendaTableBlock Day
turnIntoSingletonBlock ae =
    AgendaTableBlock
        { agendaTableBlockTitle = timestampDay $ agendaEntryTimestamp ae
        , agendaTableBlockEntries = [ae]
        }

combineBlocksByName :: Ord a => [AgendaTableBlock a] -> [AgendaTableBlock a]
combineBlocksByName = map comb . groupBy ((==) `on` agendaTableBlockTitle)
  where
    comb :: [AgendaTableBlock a] -> AgendaTableBlock a
    comb [] = error "cannot happen due to 'groupBy' above"
    comb atbs@(atb:_) =
        AgendaTableBlock
            { agendaTableBlockTitle = agendaTableBlockTitle atb
            , agendaTableBlockEntries = concatMap agendaTableBlockEntries atbs
            }
