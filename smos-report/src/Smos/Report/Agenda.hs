{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Agenda where

import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import Data.Text (Text)
import Data.Time
import Data.Validity
import GHC.Generics (Generic)
import Smos.Data
import Smos.Report.Agenda.Types
import Smos.Report.Path
import Smos.Report.Period
import Smos.Report.TimeBlock

data AgendaReport
  = AgendaReport
      { agendaReportPast :: [AgendaTableBlock Text],
        agendaReportPresent :: AgendaTodayReport,
        agendaReportFuture :: [AgendaTableBlock Text]
      }
  deriving (Show, Eq, Generic)

instance Validity AgendaReport

data AgendaTodayReport
  = AgendaTodayReport
      { agendaTodayReportEntries :: [AgendaEntry]
      }
  deriving (Show, Eq, Generic)

instance Validity AgendaTodayReport

makeAgendaReport :: ZonedTime -> Period -> TimeBlock -> [AgendaEntry] -> AgendaReport
makeAgendaReport now period tb as =
  let filteredAgenda = filter (filterPeriodLocal now period . timestampLocalTime . agendaEntryTimestamp) as
      (past, present, future) = divideIntoPastPresentFuture now $ sortAgendaEntries filteredAgenda
      pastBlocks = divideIntoAgendaTableBlocks tb past
      futureBlocks = divideIntoAgendaTableBlocks tb future
   in AgendaReport
        { agendaReportPast = pastBlocks,
          agendaReportPresent =
            AgendaTodayReport
              { agendaTodayReportEntries = present
              },
          agendaReportFuture = futureBlocks
        }

divideIntoPastPresentFuture ::
  ZonedTime -> [AgendaEntry] -> ([AgendaEntry], [AgendaEntry], [AgendaEntry])
divideIntoPastPresentFuture now =
  splitList $ \ae ->
    compare (timestampDay $ agendaEntryTimestamp ae) (localDay $ zonedTimeToLocalTime now)

splitList :: (a -> Ordering) -> [a] -> ([a], [a], [a])
splitList func = go
  where
    go [] = ([], [], [])
    go (a : as) =
      case func a of
        LT ->
          case go as of
            (xs, ys, zs) -> (a : xs, ys, zs)
        EQ ->
          case go2 as of
            (ys, zs) -> ([], a : ys, zs)
        GT -> ([], [], a : as)
    go2 [] = ([], [])
    go2 (a : as) =
      case func a of
        LT -> error "should not happen"
        EQ ->
          case go2 as of
            (ys, zs) -> (a : ys, zs)
        GT -> ([], a : as)

type AgendaTableBlock a = Block a AgendaEntry

divideIntoAgendaTableBlocks :: TimeBlock -> [AgendaEntry] -> [AgendaTableBlock Text]
divideIntoAgendaTableBlocks = divideIntoBlocks (timestampDay . agendaEntryTimestamp)

sortAgendaEntries :: [AgendaEntry] -> [AgendaEntry]
sortAgendaEntries =
  sortBy
    ( mconcat
        [ comparing (timestampLocalTime . agendaEntryTimestamp),
          comparing agendaEntryTimestampName,
          comparing agendaEntryTodoState
        ]
    )

data AgendaEntry
  = AgendaEntry
      { agendaEntryFilePath :: RootedPath,
        agendaEntryHeader :: Header,
        agendaEntryTodoState :: Maybe TodoState,
        agendaEntryTimestampName :: TimestampName,
        agendaEntryTimestamp :: Timestamp
      }
  deriving (Show, Eq, Generic)

instance Validity AgendaEntry

isDone :: Maybe TodoState -> Bool
isDone (Just "CANCELLED") = True
isDone (Just "DONE") = True
isDone (Just "FAILED") = True
isDone _ = False

makeAgendaEntry :: RootedPath -> Entry -> [AgendaEntry]
makeAgendaEntry rp e =
  flip mapMaybe (M.toList $ entryTimestamps e) $ \(tsn, ts) ->
    if isDone (entryState e)
      then Nothing
      else
        Just
          AgendaEntry
            { agendaEntryFilePath = rp,
              agendaEntryHeader = entryHeader e,
              agendaEntryTodoState = entryState e,
              agendaEntryTimestampName = tsn,
              agendaEntryTimestamp = ts
            }

fitsHistoricity :: ZonedTime -> AgendaHistoricity -> AgendaEntry -> Bool
fitsHistoricity zt ah ae =
  case ah of
    HistoricalAgenda -> True
    FutureAgenda -> timestampLocalTime (agendaEntryTimestamp ae) >= zonedTimeToLocalTime zt
