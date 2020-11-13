{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Commands.Stuck
  ( smosQueryStuck,
  )
where

import Conduit
import qualified Data.Conduit.List as C
import Data.Time
import Rainbow
import Smos.Query.Config
import Smos.Query.Formatting
import Smos.Query.OptParse.Types
import Smos.Query.Streaming
import Smos.Report.Filter
import Smos.Report.Streaming
import Smos.Report.Stuck

smosQueryStuck :: StuckSettings -> Q ()
smosQueryStuck StuckSettings {..} = do
  stuckReport <-
    fmap makeStuckReport
      $ sourceToList
      $ streamSmosProjects
        .| streamParseSmosFiles
        .| smosMFilter (FilterFst <$> stuckSetFilter)
        .| C.map (uncurry makeStuckReportEntry)
        .| C.catMaybes
  liftIO $ do
    now <- getCurrentTime
    putTableLn $ renderStuckReport now stuckReport

renderStuckReport :: UTCTime -> StuckReport -> Table
renderStuckReport now = formatAsTable . map (renderStuckReportEntry now) . stuckReportEntries

renderStuckReportEntry :: UTCTime -> StuckReportEntry -> [Chunk]
renderStuckReportEntry now StuckReportEntry {..} =
  [ pathChunk stuckReportEntryFilePath,
    mTodoStateChunk stuckReportEntryState,
    headerChunk stuckReportEntryHeader,
    maybe (chunk "") (showDaysSince 21 now) stuckReportEntryLatestChange
  ]
