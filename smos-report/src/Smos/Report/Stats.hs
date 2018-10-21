{-# LANGUAGE DeriveGeneric #-}

module Smos.Report.Stats where

import GHC.Generics (Generic)

import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import Data.Time

import Smos.Data

import Smos.Report.Period

data StatsReport = StatsReport
    { statsReportHistoricalStates :: Map (Maybe TodoState) Int
    , statsReportStates :: Map (Maybe TodoState) Int
    , statsReportFromStateTransitions :: Map (Maybe TodoState) Int
    , statsReportToStateTransitions :: Map (Maybe TodoState) Int
    , statsReportStateTransitions :: Map (Maybe TodoState, Maybe TodoState) Int
    } deriving (Show, Eq, Generic)

makeStatsReport :: ZonedTime -> Period -> [Entry] -> StatsReport
makeStatsReport now p es =
    StatsReport
        { statsReportStates = getCount $ mapMaybe (entryStateInPeriod now p) es
        , statsReportHistoricalStates =
              getCount $ historicalStatesInPeriod now p es
        , statsReportFromStateTransitions =
              getCount $ fromStateTransitionsInPeriod now p es
        , statsReportToStateTransitions =
              getCount $ toStateTransitionsInPeriod now p es
        , statsReportStateTransitions =
              getCount $ stateTransitionsInPeriod now p es
        }

withinPeriod :: ZonedTime -> Period -> StateHistoryEntry -> Bool
withinPeriod now p = filterPeriod now p . stateHistoryEntryTimestamp

stateHistoryEntryInPeriod ::
       ZonedTime -> Period -> StateHistoryEntry -> Maybe (Maybe TodoState)
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
         mapMaybe (stateHistoryEntryInPeriod now p) .
         unStateHistory . entryStateHistory)

fromStateTransitionsInPeriod ::
       ZonedTime -> Period -> [Entry] -> [Maybe TodoState]
fromStateTransitionsInPeriod now p = map fst . stateTransitionsInPeriod now p

toStateTransitionsInPeriod ::
       ZonedTime -> Period -> [Entry] -> [Maybe TodoState]
toStateTransitionsInPeriod now p = map snd . stateTransitionsInPeriod now p

stateTransitionsInPeriod ::
       ZonedTime -> Period -> [Entry] -> [(Maybe TodoState, Maybe TodoState)]
stateTransitionsInPeriod now p =
    concatMap
        (conseqMs .
         mapMaybe (stateHistoryEntryInPeriod now p) .
         unStateHistory . entryStateHistory)
  where
    conseqMs :: [Maybe a] -> [(Maybe a, Maybe a)]
    conseqMs [] = []
    conseqMs [x] =
        if p == AllTime
            then [(Nothing, x)]
            else []
    conseqMs (x:y:xs) = (y, x) : conseqMs (y : xs)

getCount :: (Ord a, Foldable f) => f a -> Map a Int
getCount = foldl (flip go) M.empty
  where
    go :: Ord a => a -> Map a Int -> Map a Int
    go i =
        flip M.alter i $ \mv ->
            case mv of
                Nothing -> Just 1
                Just n -> Just $ n + 1
