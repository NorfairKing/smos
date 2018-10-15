{-# LANGUAGE DeriveGeneric #-}

module Smos.Report.Stats where

import GHC.Generics (Generic)

import qualified Data.Map as M
import Data.Map (Map)

import Smos.Data

data StatsReport = StatsReport
    { statsReportStates :: Map (Maybe TodoState) Int
    , statsReportHistoricalStates :: Map (Maybe TodoState) Int
    , statsReportStateTransitions :: Map (Maybe TodoState, Maybe TodoState) Int
    } deriving (Show, Eq, Generic)

makeStatsReport :: [Entry] -> StatsReport
makeStatsReport es =
    StatsReport
        { statsReportStates = getCount $ map entryState es
        , statsReportHistoricalStates =
              getCount $
              concatMap
                  ((Nothing :) .
                   map stateHistoryEntryNewState .
                   unStateHistory . entryStateHistory)
                  es
        , statsReportStateTransitions =
              getCount $
              concatMap
                  (conseqMs .
                   map stateHistoryEntryNewState .
                   unStateHistory . entryStateHistory)
                  es
        }
  where
    conseqMs :: [Maybe a] -> [(Maybe a, Maybe a)]
    conseqMs [] = []
    conseqMs [x] = [(Nothing, x)]
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
