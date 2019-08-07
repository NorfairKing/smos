{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Query.Stats where

import qualified Data.Map as M
import Data.Map (Map)
import Data.Text (Text)
import Data.Time

import Conduit

import Rainbow

import Smos.Data

import Smos.Report.Path

import Smos.Report.Stats
import Smos.Report.Streaming

import Smos.Query.Config
import Smos.Query.Formatting
import Smos.Query.OptParse.Types
import Smos.Query.Streaming

stats :: StatsSettings -> Q ()
stats StatsSettings {..} = do
  now <- liftIO getZonedTime
  ad <- askArchiveDir
  pd <- askProjectsDir
  apd <- askArchivedProjectsDir
  let src =
        StatsReportContext
          { statsReportContextNow = now
          , statsReportContextPeriod = statsSetPeriod
          , statsReportContextArchiveDir = ad
          , statsReportContextProjectsDir = pd
          , statsReportContextArchivedProjectsDir = apd
          }
  sr <-
    runConduit $
    streamAllSmosFiles .| parseSmosFiles .| printShouldPrint PrintWarning .|
    accumulateStatsReport src
  liftIO $ putTableLn $ renderStatsReport sr

accumulateStatsReport :: StatsReportContext -> ConduitT (RootedPath, SmosFile) Void Q StatsReport
accumulateStatsReport src = go mempty
  where
    go !sr = do
      mf <- await
      case mf of
        Nothing -> return sr
        Just (rp, sf) -> go $ sr <> makeStatsReport src rp sf

renderStatsReport :: StatsReport -> Table
renderStatsReport StatsReport {..} =
  mconcat
    [ renderStateStatsReport statsReportStateStatsReport
    , renderProjectsStatsReport statsReportProjectStatsReport
    ]

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
formatReportStates m = flip map (M.toList m) $ \(mts, i) -> [mTodoStateChunk mts, intChunk i]

formatReportStateTransitions :: Map (Maybe TodoState, Maybe TodoState) Int -> [[Chunk Text]]
formatReportStateTransitions m =
  flip map (M.toList m) $ \((mts1, mts2), i) ->
    [mTodoStateChunk mts1, mTodoStateChunk mts2, intChunk i]

formatReportFromStateTransitions :: Map (Maybe TodoState) Int -> [[Chunk Text]]
formatReportFromStateTransitions m =
  flip map (M.toList m) $ \(mts, i) -> [mTodoStateChunk mts, chunk "(any)", intChunk i]

formatReportToStateTransitions :: Map (Maybe TodoState) Int -> [[Chunk Text]]
formatReportToStateTransitions m =
  flip map (M.toList m) $ \(mts, i) -> [chunk "(any)", mTodoStateChunk mts, intChunk i]

renderProjectsStatsReport :: ProjectStatsReport -> Table
renderProjectsStatsReport ProjectStatsReport {..} =
  formatAsTable $
  [ [fore white $ chunk "All Projects"]
  , [chunk "Current Projects", intChunk projectStatsReportCurrentProjects]
  , [chunk "Archived Projects", intChunk projectStatsReportArchivedProjects]
  , [chunk "Total Projects", intChunk projectStatsReportTotalProjects]
  , [chunk ""]
  , [fore white $ chunk "Files"]
  , [chunk "Current Files", intChunk projectStatsReportCurrentFiles]
  , [chunk "Archived Files", intChunk projectStatsReportArchivedFiles]
  , [chunk "Total Files", intChunk projectStatsReportTotalFiles]
  ]
