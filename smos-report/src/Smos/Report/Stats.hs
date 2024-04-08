{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.Stats where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Time.Zones
import Data.Tree
import Path
import Smos.Data
import Smos.Report.Period

data StatsReportContext = StatsReportContext
  { statsReportContextTimeZone :: !TZ,
    statsReportContextInterval :: !Interval,
    statsReportContextWorkflowDir :: !(Path Abs Dir),
    statsReportContextArchiveDir :: !(Path Abs Dir),
    statsReportContextProjectsDir :: !(Path Abs Dir),
    statsReportContextArchivedProjectsDir :: !(Path Abs Dir)
  }

data StatsReport = StatsReport
  { statsReportProjectStatsReport :: !ProjectStatsReport,
    statsReportStateStatsReport :: !StateStatsReport
  }

instance Semigroup StatsReport where
  sr1 <> sr2 =
    StatsReport
      { statsReportProjectStatsReport =
          statsReportProjectStatsReport sr1 <> statsReportProjectStatsReport sr2,
        statsReportStateStatsReport =
          statsReportStateStatsReport sr1 <> statsReportStateStatsReport sr2
      }

instance Monoid StatsReport where
  mempty =
    StatsReport {statsReportProjectStatsReport = mempty, statsReportStateStatsReport = mempty}

makeStatsReport :: StatsReportContext -> Path Rel File -> SmosFile -> StatsReport
makeStatsReport src@StatsReportContext {..} rp sf =
  StatsReport
    { statsReportProjectStatsReport = makeProjectsStatsReport src rp sf,
      statsReportStateStatsReport =
        makeStateStatsReport statsReportContextTimeZone statsReportContextInterval $
          concatMap flatten $
            smosFileForest sf
    }

makeProjectsStatsReport :: StatsReportContext -> Path Rel File -> SmosFile -> ProjectStatsReport
makeProjectsStatsReport StatsReportContext {..} rp sf =
  ProjectStatsReport
    { projectStatsReportArchivedProjects = countIf $ active && isArchivedProject,
      projectStatsReportCurrentProjects = countIf $ active && isProject,
      projectStatsReportTotalProjects = countIf $ active && (isArchivedProject || isProject),
      projectStatsReportArchivedFiles = countIf $ active && isArchived,
      projectStatsReportCurrentFiles = countIf $ active && not isArchived,
      projectStatsReportTotalFiles = countIf active
    }
  where
    active = smosFileActiveDuringPeriod statsReportContextTimeZone statsReportContextInterval sf
    fullPath = (statsReportContextWorkflowDir </>)
    isArchived = isProperPrefixOf statsReportContextArchiveDir $ fullPath rp
    isArchivedProject = isProperPrefixOf statsReportContextArchivedProjectsDir $ fullPath rp
    isProject = isProperPrefixOf statsReportContextProjectsDir $ fullPath rp
    countIf b =
      if b
        then 1
        else 0

smosFileActiveDuringPeriod :: TZ -> Interval -> SmosFile -> Bool
smosFileActiveDuringPeriod zone interval sf =
  (interval == EverythingInterval)
    || not (null $ stateHistoryEntriesInPeriod zone interval $ concatMap flatten $ smosFileForest sf)

stateHistoryEntriesInPeriod :: TZ -> Interval -> [Entry] -> [StateHistoryEntry]
stateHistoryEntriesInPeriod zone interval = concatMap go
  where
    go :: Entry -> [StateHistoryEntry]
    go = mapMaybe (stateHistoryEntryInPeriod zone interval) . unStateHistory . entryStateHistory

data ProjectStatsReport = ProjectStatsReport
  { projectStatsReportCurrentProjects :: Int,
    projectStatsReportArchivedProjects :: Int,
    projectStatsReportTotalProjects :: Int,
    projectStatsReportCurrentFiles :: Int,
    projectStatsReportArchivedFiles :: Int,
    projectStatsReportTotalFiles :: Int
  }

instance Semigroup ProjectStatsReport where
  psr1 <> psr2 =
    ProjectStatsReport
      { projectStatsReportArchivedProjects =
          projectStatsReportArchivedProjects psr1 + projectStatsReportArchivedProjects psr2,
        projectStatsReportCurrentProjects =
          projectStatsReportCurrentProjects psr1 + projectStatsReportCurrentProjects psr2,
        projectStatsReportTotalProjects =
          projectStatsReportTotalProjects psr1 + projectStatsReportTotalProjects psr2,
        projectStatsReportArchivedFiles =
          projectStatsReportArchivedFiles psr1 + projectStatsReportArchivedFiles psr2,
        projectStatsReportCurrentFiles =
          projectStatsReportCurrentFiles psr1 + projectStatsReportCurrentFiles psr2,
        projectStatsReportTotalFiles =
          projectStatsReportTotalFiles psr1 + projectStatsReportTotalFiles psr2
      }

instance Monoid ProjectStatsReport where
  mempty =
    ProjectStatsReport
      { projectStatsReportArchivedProjects = 0,
        projectStatsReportCurrentProjects = 0,
        projectStatsReportTotalProjects = 0,
        projectStatsReportArchivedFiles = 0,
        projectStatsReportCurrentFiles = 0,
        projectStatsReportTotalFiles = 0
      }
  mappend = (<>)

data StateStatsReport = StateStatsReport
  { stateStatsReportHistoricalStates :: !(Map (Maybe TodoState) Int),
    stateStatsReportStates :: !(Map (Maybe TodoState) Int),
    stateStatsReportFromStateTransitions :: !(Map (Maybe TodoState) Int),
    stateStatsReportToStateTransitions :: !(Map (Maybe TodoState) Int),
    stateStatsReportStateTransitions :: !(Map (Maybe TodoState, Maybe TodoState) Int)
  }

instance Semigroup StateStatsReport where
  sr1 <> sr2 =
    StateStatsReport
      { stateStatsReportHistoricalStates =
          addMapOfInts (stateStatsReportHistoricalStates sr1) (stateStatsReportHistoricalStates sr2),
        stateStatsReportStates =
          addMapOfInts (stateStatsReportStates sr1) (stateStatsReportStates sr2),
        stateStatsReportFromStateTransitions =
          addMapOfInts
            (stateStatsReportFromStateTransitions sr1)
            (stateStatsReportFromStateTransitions sr2),
        stateStatsReportToStateTransitions =
          addMapOfInts
            (stateStatsReportToStateTransitions sr1)
            (stateStatsReportToStateTransitions sr2),
        stateStatsReportStateTransitions =
          addMapOfInts (stateStatsReportStateTransitions sr1) (stateStatsReportStateTransitions sr2)
      }

addMapOfInts :: (Ord a) => Map a Int -> Map a Int -> Map a Int
addMapOfInts = M.unionWith (+)

instance Monoid StateStatsReport where
  mempty =
    StateStatsReport
      { stateStatsReportHistoricalStates = M.empty,
        stateStatsReportStates = M.empty,
        stateStatsReportFromStateTransitions = M.empty,
        stateStatsReportToStateTransitions = M.empty,
        stateStatsReportStateTransitions = M.empty
      }
  mappend = (<>)

makeStateStatsReport :: TZ -> Interval -> [Entry] -> StateStatsReport
makeStateStatsReport zone interval es =
  StateStatsReport
    { stateStatsReportStates = getCount $ mapMaybe (entryStateInPeriod zone interval) es,
      stateStatsReportHistoricalStates = getCount $ historicalStatesInPeriod zone interval es,
      stateStatsReportFromStateTransitions = getCount $ fromStateTransitionsInPeriod zone interval es,
      stateStatsReportToStateTransitions = getCount $ toStateTransitionsInPeriod zone interval es,
      stateStatsReportStateTransitions = getCount $ stateTransitionsInPeriod zone interval es
    }

withinPeriod :: TZ -> Interval -> StateHistoryEntry -> Bool
withinPeriod zone interval = filterIntervalUTCTime zone interval . stateHistoryEntryTimestamp

stateHistoryEntryInPeriod :: TZ -> Interval -> StateHistoryEntry -> Maybe StateHistoryEntry
stateHistoryEntryInPeriod zone interval tse =
  if withinPeriod zone interval tse
    then Just tse
    else Nothing

stateHistoryStateInPeriod :: TZ -> Interval -> StateHistoryEntry -> Maybe (Maybe TodoState)
stateHistoryStateInPeriod zone interval tse =
  stateHistoryEntryNewState <$> stateHistoryEntryInPeriod zone interval tse

entryStateInPeriod :: TZ -> Interval -> Entry -> Maybe (Maybe TodoState)
entryStateInPeriod zone interval e =
  case (interval, unStateHistory $ entryStateHistory e) of
    (EverythingInterval, []) -> Just Nothing
    (_, []) -> Nothing
    (_, tse : _) -> stateHistoryStateInPeriod zone interval tse

historicalStatesInPeriod :: TZ -> Interval -> [Entry] -> [Maybe TodoState]
historicalStatesInPeriod zone interval =
  concatMap
    ( ( if interval == EverythingInterval
          then (Nothing :)
          else id
      )
        . mapMaybe (stateHistoryStateInPeriod zone interval)
        . unStateHistory
        . entryStateHistory
    )

fromStateTransitionsInPeriod :: TZ -> Interval -> [Entry] -> [Maybe TodoState]
fromStateTransitionsInPeriod zone interval = map fst . stateTransitionsInPeriod zone interval

toStateTransitionsInPeriod :: TZ -> Interval -> [Entry] -> [Maybe TodoState]
toStateTransitionsInPeriod zone interval = map snd . stateTransitionsInPeriod zone interval

stateTransitionsInPeriod :: TZ -> Interval -> [Entry] -> [(Maybe TodoState, Maybe TodoState)]
stateTransitionsInPeriod zone interval = concatMap go
  where
    go :: Entry -> [(Maybe TodoState, Maybe TodoState)]
    go = go' . unStateHistory . entryStateHistory
    go' :: [StateHistoryEntry] -> [(Maybe TodoState, Maybe TodoState)]
    go' [] = []
    go' [she] =
      case stateHistoryStateInPeriod zone interval she of
        Nothing -> []
        Just mts -> [(Nothing, mts)]
    go' (x : y : xs) =
      case (,) <$> stateHistoryStateInPeriod zone interval x <*> stateHistoryStateInPeriod zone interval y of
        Just (tsx, tsy) -> (tsy, tsx) : go' (y : xs)
        _ -> go' (y : xs)

getCount :: (Ord a, Foldable f) => f a -> Map a Int
getCount = foldl (flip go) M.empty
  where
    go :: (Ord a) => a -> Map a Int -> Map a Int
    go i =
      flip M.alter i $ \case
        Nothing -> Just 1
        Just n -> Just $ n + 1
