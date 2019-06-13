{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Query.Stats where

import qualified Data.Map as M
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time

import Conduit
import qualified Data.Conduit.Combinators as C
import Rainbow

import Smos.Data

import Smos.Report.Query
import Smos.Report.Stats
import Smos.Report.Streaming

import Smos.Query.Config
import Smos.Query.Formatting
import Smos.Query.OptParse.Types

stats :: StatsSettings -> Q ()
stats StatsSettings {..} = do
  wd <- askWorkDir
  liftIO $ do
    now <- getZonedTime
    es <-
      sourceToList $
      sourceFilesInNonHiddenDirsRecursively wd .| filterSmosFiles .|
      parseSmosFiles .|
      printShouldPrint PrintWarning .|
      smosFileCursors .|
      C.filter
        (\(rp, fc) -> maybe True (\f -> filterPredicate f rp fc) statsSetFilter) .|
      smosCursorCurrents .|
      C.map snd
    putTableLn $ renderStatsReport $ makeStatsReport now statsSetPeriod es

renderStatsReport :: StatsReport -> Table
renderStatsReport StatsReport {..} =
  formatAsTable $
  concat
    [ [[fore white $ chunk "Historical states"]]
    , formatReportStates statsReportHistoricalStates
    , [[chunk ""]]
    , [[fore white $ chunk "Current states"]]
    , formatReportStates statsReportStates
    , [[chunk ""]]
    , [[fore white $ chunk "State Transitions"]]
    , formatReportStateTransitions statsReportStateTransitions
    , [[chunk ""]]
    , [[fore white $ chunk "State Transitions (from)"]]
    , formatReportFromStateTransitions statsReportFromStateTransitions
    , [[chunk ""]]
    , [[fore white $ chunk "State Transitions (to)"]]
    , formatReportToStateTransitions statsReportToStateTransitions
    , [[chunk ""]]
    ]

formatReportStates :: Map (Maybe TodoState) Int -> [[Chunk Text]]
formatReportStates m =
  flip map (M.toList m) $ \(mts, i) ->
    [mTodoStateChunk mts, chunk $ T.pack $ show i]

formatReportStateTransitions ::
     Map (Maybe TodoState, Maybe TodoState) Int -> [[Chunk Text]]
formatReportStateTransitions m =
  flip map (M.toList m) $ \((mts1, mts2), i) ->
    [mTodoStateChunk mts1, mTodoStateChunk mts2, chunk $ T.pack $ show i]

formatReportFromStateTransitions :: Map (Maybe TodoState) Int -> [[Chunk Text]]
formatReportFromStateTransitions m =
  flip map (M.toList m) $ \(mts, i) ->
    [mTodoStateChunk mts, chunk "(any)", chunk $ T.pack $ show i]

formatReportToStateTransitions :: Map (Maybe TodoState) Int -> [[Chunk Text]]
formatReportToStateTransitions m =
  flip map (M.toList m) $ \(mts, i) ->
    [chunk "(any)", mTodoStateChunk mts, chunk $ T.pack $ show i]
