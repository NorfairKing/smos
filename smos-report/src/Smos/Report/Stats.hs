{-# LANGUAGE DeriveGeneric #-}

module Smos.Report.Stats where

import GHC.Generics (Generic)

import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import Data.Time
import Data.Tree

import Smos.Data

import Smos.Report.Path
import Smos.Report.Period

data StatsReport =
  StatsReport
    { statsReportStateStatsReport :: StateStatsReport
    }
  deriving (Show, Eq, Generic)

instance Semigroup StatsReport where
  sr1 <> sr2 =
    StatsReport
      { statsReportStateStatsReport =
          statsReportStateStatsReport sr1 <> statsReportStateStatsReport sr2
      }

instance Monoid StatsReport where
  mempty = StatsReport { statsReportStateStatsReport=mempty}

makeStatsReport :: ZonedTime -> Period -> RootedPath -> SmosFile -> StatsReport
makeStatsReport now p _ sf =
  StatsReport
    { statsReportStateStatsReport =
        makeStateStatsReport now p $ concatMap flatten $ smosFileForest sf
    }

data StateStatsReport =
  StateStatsReport
    { stateStatsReportHistoricalStates :: Map (Maybe TodoState) Int
    , stateStatsReportStates :: Map (Maybe TodoState) Int
    , stateStatsReportFromStateTransitions :: Map (Maybe TodoState) Int
    , stateStatsReportToStateTransitions :: Map (Maybe TodoState) Int
    , stateStatsReportStateTransitions :: Map (Maybe TodoState, Maybe TodoState) Int
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

stateHistoryEntryInPeriod :: ZonedTime -> Period -> StateHistoryEntry -> Maybe (Maybe TodoState)
stateHistoryEntryInPeriod now p tse =
  if withinPeriod now p tse
    then Just (stateHistoryEntryNewState tse)
    else Nothing

entryStateInPeriod :: ZonedTime -> Period -> Entry -> Maybe (Maybe TodoState)
entryStateInPeriod now p e =
  case (p, unStateHistory $ entryStateHistory e) of
    (AllTime, []) -> Just Nothing
    (_, []) -> Nothing
    (_, (tse:_)) -> stateHistoryEntryInPeriod now p tse

historicalStatesInPeriod :: ZonedTime -> Period -> [Entry] -> [Maybe TodoState]
historicalStatesInPeriod now p =
  concatMap
    ((if p == AllTime
        then (Nothing :)
        else id) .
     mapMaybe (stateHistoryEntryInPeriod now p) . unStateHistory . entryStateHistory)

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
      case stateHistoryEntryInPeriod now p she of
        Nothing -> []
        Just mts -> [(Nothing, mts)]
    go' (x:y:xs) =
      case (,) <$> stateHistoryEntryInPeriod now p x <*> stateHistoryEntryInPeriod now p y of
        Just (tsx, tsy) -> (tsy, tsx) : go' (y : xs)
        _ -> go' (y : xs)

getCount :: (Ord a, Foldable f) => f a -> Map a Int
getCount = foldl (flip go) M.empty
  where
    go :: Ord a => a -> Map a Int -> Map a Int
    go i =
      flip M.alter i $ \mv ->
        case mv of
          Nothing -> Just 1
          Just n -> Just $ n + 1
