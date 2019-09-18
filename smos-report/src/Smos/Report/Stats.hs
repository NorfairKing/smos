{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.Stats where

import GHC.Generics (Generic)

import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import Data.Time
import Data.Tree
import Path

import Smos.Data

import Smos.Report.Path
import Smos.Report.Period

data StatsReportContext =
  StatsReportContext
    { statsReportContextNow :: ZonedTime
    , statsReportContextPeriod :: Period
    , statsReportContextArchiveDir :: Path Abs Dir
    , statsReportContextProjectsDir :: Path Abs Dir
    , statsReportContextArchivedProjectsDir :: Path Abs Dir
    }
  deriving (Show, Generic)

data StatsReport =
  StatsReport
    { statsReportProjectStatsReport :: !ProjectStatsReport
    , statsReportStateStatsReport :: !StateStatsReport
    }
  deriving (Show, Eq, Generic)

instance Semigroup StatsReport where
  sr1 <> sr2 =
    StatsReport
      { statsReportProjectStatsReport =
          statsReportProjectStatsReport sr1 <> statsReportProjectStatsReport sr2
      , statsReportStateStatsReport =
          statsReportStateStatsReport sr1 <> statsReportStateStatsReport sr2
      }

instance Monoid StatsReport where
  mempty =
    StatsReport {statsReportProjectStatsReport = mempty, statsReportStateStatsReport = mempty}

makeStatsReport :: StatsReportContext -> RootedPath -> SmosFile -> StatsReport
makeStatsReport src@StatsReportContext {..} rp sf =
  StatsReport
    { statsReportProjectStatsReport = makeProjectsStatsReport src rp sf
    , statsReportStateStatsReport =
        makeStateStatsReport statsReportContextNow statsReportContextPeriod $
        concatMap flatten $ smosFileForest sf
    }

makeProjectsStatsReport :: StatsReportContext -> RootedPath -> SmosFile -> ProjectStatsReport
makeProjectsStatsReport StatsReportContext {..} rp sf =
  ProjectStatsReport
    { projectStatsReportArchivedProjects = countIf $ active && isArchivedProject
    , projectStatsReportCurrentProjects = countIf $ active && isProject
    , projectStatsReportTotalProjects = countIf $ active && (isArchivedProject || isProject)
    , projectStatsReportArchivedFiles = countIf $ active && isArchived
    , projectStatsReportCurrentFiles = countIf $ active && not isArchived
    , projectStatsReportTotalFiles = countIf active
    }
  where
    active = smosFileActiveDuringPeriod statsReportContextNow statsReportContextPeriod sf
    isArchived = isProperPrefixOf statsReportContextArchiveDir $ resolveRootedPath rp
    isArchivedProject =
      isProperPrefixOf statsReportContextArchivedProjectsDir $ resolveRootedPath rp
    isProject = isProperPrefixOf statsReportContextProjectsDir $ resolveRootedPath rp
    countIf b =
      if b
        then 1
        else 0

smosFileActiveDuringPeriod :: ZonedTime -> Period -> SmosFile -> Bool
smosFileActiveDuringPeriod now p sf =
  (p == AllTime) ||
  not (null $ stateHistoryEntriesInPeriod now p $ concatMap flatten $ smosFileForest sf)

stateHistoryEntriesInPeriod :: ZonedTime -> Period -> [Entry] -> [StateHistoryEntry]
stateHistoryEntriesInPeriod now p = concatMap go
  where
    go :: Entry -> [StateHistoryEntry]
    go = mapMaybe (stateHistoryEntryInPeriod now p) . unStateHistory . entryStateHistory

data ProjectStatsReport =
  ProjectStatsReport
    { projectStatsReportCurrentProjects :: Int
    , projectStatsReportArchivedProjects :: Int
    , projectStatsReportTotalProjects :: Int
    , projectStatsReportCurrentFiles :: Int
    , projectStatsReportArchivedFiles :: Int
    , projectStatsReportTotalFiles :: Int
    }
  deriving (Show, Eq, Generic)

instance Semigroup ProjectStatsReport where
  psr1 <> psr2 =
    ProjectStatsReport
      { projectStatsReportArchivedProjects =
          projectStatsReportArchivedProjects psr1 + projectStatsReportArchivedProjects psr2
      , projectStatsReportCurrentProjects =
          projectStatsReportCurrentProjects psr1 + projectStatsReportCurrentProjects psr2
      , projectStatsReportTotalProjects =
          projectStatsReportTotalProjects psr1 + projectStatsReportTotalProjects psr2
      , projectStatsReportArchivedFiles =
          projectStatsReportArchivedFiles psr1 + projectStatsReportArchivedFiles psr2
      , projectStatsReportCurrentFiles =
          projectStatsReportCurrentFiles psr1 + projectStatsReportCurrentFiles psr2
      , projectStatsReportTotalFiles =
          projectStatsReportTotalFiles psr1 + projectStatsReportTotalFiles psr2
      }

instance Monoid ProjectStatsReport where
  mempty =
    ProjectStatsReport
      { projectStatsReportArchivedProjects = 0
      , projectStatsReportCurrentProjects = 0
      , projectStatsReportTotalProjects = 0
      , projectStatsReportArchivedFiles = 0
      , projectStatsReportCurrentFiles = 0
      , projectStatsReportTotalFiles = 0
      }
  mappend = (<>)

data StateStatsReport =
  StateStatsReport
    { stateStatsReportHistoricalStates :: !(Map (Maybe TodoState) Int)
    , stateStatsReportStates :: !(Map (Maybe TodoState) Int)
    , stateStatsReportFromStateTransitions :: !(Map (Maybe TodoState) Int)
    , stateStatsReportToStateTransitions :: !(Map (Maybe TodoState) Int)
    , stateStatsReportStateTransitions :: !(Map (Maybe TodoState, Maybe TodoState) Int)
    }
  deriving (Show, Eq, Generic)

instance Semigroup StateStatsReport where
  sr1 <> sr2 =
    StateStatsReport
      { stateStatsReportHistoricalStates =
          addMapOfInts (stateStatsReportHistoricalStates sr1) (stateStatsReportHistoricalStates sr2)
      , stateStatsReportStates =
          addMapOfInts (stateStatsReportStates sr1) (stateStatsReportStates sr2)
      , stateStatsReportFromStateTransitions =
          addMapOfInts
            (stateStatsReportFromStateTransitions sr1)
            (stateStatsReportFromStateTransitions sr2)
      , stateStatsReportToStateTransitions =
          addMapOfInts
            (stateStatsReportToStateTransitions sr1)
            (stateStatsReportToStateTransitions sr2)
      , stateStatsReportStateTransitions =
          addMapOfInts (stateStatsReportStateTransitions sr1) (stateStatsReportStateTransitions sr2)
      }

addMapOfInts :: Ord a => Map a Int -> Map a Int -> Map a Int
addMapOfInts = M.unionWith (+)

instance Monoid StateStatsReport where
  mempty =
    StateStatsReport
      { stateStatsReportHistoricalStates = M.empty
      , stateStatsReportStates = M.empty
      , stateStatsReportFromStateTransitions = M.empty
      , stateStatsReportToStateTransitions = M.empty
      , stateStatsReportStateTransitions = M.empty
      }
  mappend = (<>)

makeStateStatsReport :: ZonedTime -> Period -> [Entry] -> StateStatsReport
makeStateStatsReport now p es =
  StateStatsReport
    { stateStatsReportStates = getCount $ mapMaybe (entryStateInPeriod now p) es
    , stateStatsReportHistoricalStates = getCount $ historicalStatesInPeriod now p es
    , stateStatsReportFromStateTransitions = getCount $ fromStateTransitionsInPeriod now p es
    , stateStatsReportToStateTransitions = getCount $ toStateTransitionsInPeriod now p es
    , stateStatsReportStateTransitions = getCount $ stateTransitionsInPeriod now p es
    }

withinPeriod :: ZonedTime -> Period -> StateHistoryEntry -> Bool
withinPeriod now p = filterPeriod now p . stateHistoryEntryTimestamp

stateHistoryEntryInPeriod :: ZonedTime -> Period -> StateHistoryEntry -> Maybe StateHistoryEntry
stateHistoryEntryInPeriod now p tse =
  if withinPeriod now p tse
    then Just tse
    else Nothing

stateHistoryStateInPeriod :: ZonedTime -> Period -> StateHistoryEntry -> Maybe (Maybe TodoState)
stateHistoryStateInPeriod now p tse =
  stateHistoryEntryNewState <$> stateHistoryEntryInPeriod now p tse

entryStateInPeriod :: ZonedTime -> Period -> Entry -> Maybe (Maybe TodoState)
entryStateInPeriod now p e =
  case (p, unStateHistory $ entryStateHistory e) of
    (AllTime, []) -> Just Nothing
    (_, []) -> Nothing
    (_, tse:_) -> stateHistoryStateInPeriod now p tse

historicalStatesInPeriod :: ZonedTime -> Period -> [Entry] -> [Maybe TodoState]
historicalStatesInPeriod now p =
  concatMap
    ((if p == AllTime
        then (Nothing :)
        else id) .
     mapMaybe (stateHistoryStateInPeriod now p) . unStateHistory . entryStateHistory)

fromStateTransitionsInPeriod :: ZonedTime -> Period -> [Entry] -> [Maybe TodoState]
fromStateTransitionsInPeriod now p = map fst . stateTransitionsInPeriod now p

toStateTransitionsInPeriod :: ZonedTime -> Period -> [Entry] -> [Maybe TodoState]
toStateTransitionsInPeriod now p = map snd . stateTransitionsInPeriod now p

stateTransitionsInPeriod :: ZonedTime -> Period -> [Entry] -> [(Maybe TodoState, Maybe TodoState)]
stateTransitionsInPeriod now p = concatMap go
  where
    go :: Entry -> [(Maybe TodoState, Maybe TodoState)]
    go = go' . unStateHistory . entryStateHistory
    go' :: [StateHistoryEntry] -> [(Maybe TodoState, Maybe TodoState)]
    go' [] = []
    go' [she] =
      case stateHistoryStateInPeriod now p she of
        Nothing -> []
        Just mts -> [(Nothing, mts)]
    go' (x:y:xs) =
      case (,) <$> stateHistoryStateInPeriod now p x <*> stateHistoryStateInPeriod now p y of
        Just (tsx, tsy) -> (tsy, tsx) : go' (y : xs)
        _ -> go' (y : xs)

getCount :: (Ord a, Foldable f) => f a -> Map a Int
getCount = foldl (flip go) M.empty
  where
    go :: Ord a => a -> Map a Int -> Map a Int
    go i =
      flip M.alter i $ \case
        Nothing -> Just 1
        Just n -> Just $ n + 1
