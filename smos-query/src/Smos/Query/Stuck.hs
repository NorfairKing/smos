{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Query.Stuck where

import Data.Text (Text)
import Data.Time

import Conduit
import qualified Data.Conduit.List as C
import Rainbow

import Smos.Report.Filter
import Smos.Report.Streaming
import Smos.Report.Stuck

import Smos.Query.Config
import Smos.Query.Formatting
import Smos.Query.OptParse.Types
import Smos.Query.Streaming
import Smos.Query.Waiting

stuck :: StuckSettings -> Q ()
stuck StuckSettings {..} = do
  stuckReport <-
    fmap makeStuckReport $
    sourceToList $
    streamSmosProjects .| parseSmosFiles .| printShouldPrint PrintWarning .|
    smosMFilter (FilterFst <$> stuckSetFilter) .|
    C.map (uncurry makeStuckReportEntry) .|
    C.catMaybes
  liftIO $ do
    now <- getCurrentTime
    putTableLn $ renderStuckReport now stuckReport

renderStuckReport :: UTCTime -> StuckReport -> Table
renderStuckReport now = formatAsTable . map (renderStuckReportEntry now) . stucReportEntries

renderStuckReportEntry :: UTCTime -> StuckReportEntry -> [Chunk Text]
renderStuckReportEntry now StuckReportEntry {..} =
  [ rootedPathChunk stuckReportEntryFilePath
  , mTodoStateChunk stuckReportEntryState
  , headerChunk stuckReportEntryHeader
  , maybe (chunk "") (showDaysSince 21 now) stuckReportEntryLatestChange
  ]
