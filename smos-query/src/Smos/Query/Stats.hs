{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Query.Stats where

import qualified Data.Map as M
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time

import Conduit

-- import qualified Data.Conduit.Combinators as C
import Rainbow

import Smos.Data

import Smos.Report.Path
import Smos.Report.Period
import Smos.Report.Stats
import Smos.Report.Streaming

import Smos.Query.Config
import Smos.Query.Formatting
import Smos.Query.OptParse.Types
import Smos.Query.Streaming

stats :: StatsSettings -> Q ()
stats StatsSettings {..} = do
  now <- liftIO getZonedTime
  sr <-
    runConduit $
    streamSmosFiles .| parseSmosFiles .| printShouldPrint PrintWarning .|
    accumulateStatsReport now statsSetPeriod
  liftIO $ putTableLn $ renderStatsReport sr

accumulateStatsReport :: ZonedTime -> Period -> ConduitT (RootedPath, SmosFile) Void Q StatsReport
accumulateStatsReport zt p = go mempty
  where
    go sr = do
      mf <- await
      case mf of
        Nothing -> return sr
        Just (rp, sf) -> go $ sr <> makeStatsReport zt p rp sf


renderStatsReport :: StatsReport -> Table
renderStatsReport StatsReport {..} = renderStateStatsReport statsReportStateStatsReport

renderStateStatsReport :: StateStatsReport -> Table
renderStateStatsReport StateStatsReport {..} =
  formatAsTable $
  concat
    [ [[fore white $ chunk "Historical states"]]
    , formatReportStates stateStatsReportHistoricalStates
    , [[chunk ""]]
    , [[fore white $ chunk "Current states"]]
    , formatReportStates stateStatsReportStates
    , [[chunk ""]]
    , [[fore white $ chunk "State Transitions"]]
    , formatReportStateTransitions stateStatsReportStateTransitions
    , [[chunk ""]]
    , [[fore white $ chunk "State Transitions (from)"]]
    , formatReportFromStateTransitions stateStatsReportFromStateTransitions
    , [[chunk ""]]
    , [[fore white $ chunk "State Transitions (to)"]]
    , formatReportToStateTransitions stateStatsReportToStateTransitions
    , [[chunk ""]]
    ]

formatReportStates :: Map (Maybe TodoState) Int -> [[Chunk Text]]
formatReportStates m =
  flip map (M.toList m) $ \(mts, i) -> [mTodoStateChunk mts, chunk $ T.pack $ show i]

formatReportStateTransitions :: Map (Maybe TodoState, Maybe TodoState) Int -> [[Chunk Text]]
formatReportStateTransitions m =
  flip map (M.toList m) $ \((mts1, mts2), i) ->
    [mTodoStateChunk mts1, mTodoStateChunk mts2, chunk $ T.pack $ show i]

formatReportFromStateTransitions :: Map (Maybe TodoState) Int -> [[Chunk Text]]
formatReportFromStateTransitions m =
  flip map (M.toList m) $ \(mts, i) -> [mTodoStateChunk mts, chunk "(any)", chunk $ T.pack $ show i]

formatReportToStateTransitions :: Map (Maybe TodoState) Int -> [[Chunk Text]]
formatReportToStateTransitions m =
  flip map (M.toList m) $ \(mts, i) -> [chunk "(any)", mTodoStateChunk mts, chunk $ T.pack $ show i]
