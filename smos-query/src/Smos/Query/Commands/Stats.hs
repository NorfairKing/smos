{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Commands.Stats
  ( smosQueryStats,
  )
where

import Conduit
import qualified Data.Conduit.Combinators as C
import qualified Data.Map as M
import Smos.Query.Commands.Import
import Smos.Report.Period
import Smos.Report.Stats

smosQueryStats :: StatsSettings -> Q ()
smosQueryStats StatsSettings {..} = do
  zone <- liftIO loadLocalTZ
  now <- liftIO getCurrentTime
  let today = localDay (utcToLocalTimeTZ zone now)

  wd <- askWorkflowDir
  ad <- askArchiveDir
  pd <- askProjectsDir
  apd <- askArchivedProjectsDir
  let src =
        StatsReportContext
          { statsReportContextTimeZone = zone,
            statsReportContextInterval = periodInterval today statsSetPeriod,
            statsReportContextWorkflowDir = wd,
            statsReportContextArchiveDir = ad,
            statsReportContextProjectsDir = pd,
            statsReportContextArchivedProjectsDir = apd
          }
  statsReport <-
    runConduit $
      streamAllSmosFiles
        .| streamParseSmosFiles
        .| accumulateStatsReport src
  colourSettings <- asks envColourSettings
  outputChunks $ renderStatsReport colourSettings statsReport

accumulateStatsReport :: StatsReportContext -> ConduitT (Path Rel File, SmosFile) Void Q StatsReport
accumulateStatsReport src = C.map (uncurry $ makeStatsReport src) .| C.fold

renderStatsReport :: ColourSettings -> StatsReport -> [Chunk]
renderStatsReport colourSettings StatsReport {..} =
  mconcat
    [ renderStateStatsReport colourSettings statsReportStateStatsReport,
      renderProjectsStatsReport colourSettings statsReportProjectStatsReport
    ]

renderStateStatsReport :: ColourSettings -> StateStatsReport -> [Chunk]
renderStateStatsReport colourSettings StateStatsReport {..} =
  formatAsBicolourTable colourSettings $
    concat
      [ [[fore white $ chunk "Historical states"]],
        formatReportStates stateStatsReportHistoricalStates,
        [[chunk ""]],
        [[fore white $ chunk "Current states"]],
        formatReportStates stateStatsReportStates,
        [[chunk ""]],
        [[fore white $ chunk "State Transitions"]],
        formatReportStateTransitions stateStatsReportStateTransitions,
        [[chunk ""]],
        [[fore white $ chunk "State Transitions (from)"]],
        formatReportFromStateTransitions stateStatsReportFromStateTransitions,
        [[chunk ""]],
        [[fore white $ chunk "State Transitions (to)"]],
        formatReportToStateTransitions stateStatsReportToStateTransitions,
        [[chunk ""]]
      ]

formatReportStates :: Map (Maybe TodoState) Int -> [[Chunk]]
formatReportStates m = flip map (M.toList m) $ \(mts, i) -> [mTodoStateChunk mts, intChunk i]

formatReportStateTransitions :: Map (Maybe TodoState, Maybe TodoState) Int -> [[Chunk]]
formatReportStateTransitions m =
  flip map (M.toList m) $ \((mts1, mts2), i) ->
    [mTodoStateChunk mts1, mTodoStateChunk mts2, intChunk i]

formatReportFromStateTransitions :: Map (Maybe TodoState) Int -> [[Chunk]]
formatReportFromStateTransitions m =
  flip map (M.toList m) $ \(mts, i) -> [mTodoStateChunk mts, chunk "(any)", intChunk i]

formatReportToStateTransitions :: Map (Maybe TodoState) Int -> [[Chunk]]
formatReportToStateTransitions m =
  flip map (M.toList m) $ \(mts, i) -> [chunk "(any)", mTodoStateChunk mts, intChunk i]

renderProjectsStatsReport :: ColourSettings -> ProjectStatsReport -> [Chunk]
renderProjectsStatsReport colourSettings ProjectStatsReport {..} =
  formatAsBicolourTable
    colourSettings
    [ [fore white $ chunk "All Projects"],
      [chunk "Current Projects", intChunk projectStatsReportCurrentProjects],
      [chunk "Archived Projects", intChunk projectStatsReportArchivedProjects],
      [chunk "Total Projects", intChunk projectStatsReportTotalProjects],
      [chunk ""],
      [fore white $ chunk "Files"],
      [chunk "Current Files", intChunk projectStatsReportCurrentFiles],
      [chunk "Archived Files", intChunk projectStatsReportArchivedFiles],
      [chunk "Total Files", intChunk projectStatsReportTotalFiles]
    ]
