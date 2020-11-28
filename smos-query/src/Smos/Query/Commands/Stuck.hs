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
  now <- liftIO getZonedTime
  stuckReport <-
    fmap makeStuckReport
      $ sourceToList
      $ streamSmosProjects
        .| streamParseSmosProjects
        .| smosMFilter (FilterFst <$> stuckSetFilter)
        .| C.map (uncurry (makeStuckReportEntry (zonedTimeZone now)))
        .| C.catMaybes
  putTableLn $ renderStuckReport (zonedTimeToUTC now) stuckReport

renderStuckReport :: UTCTime -> StuckReport -> Table
renderStuckReport now = formatAsTable . map (renderStuckReportEntry now) . stuckReportEntries

renderStuckReportEntry :: UTCTime -> StuckReportEntry -> [Chunk]
renderStuckReportEntry now StuckReportEntry {..} =
  [ pathChunk stuckReportEntryFilePath,
    mTodoStateChunk stuckReportEntryState,
    headerChunk stuckReportEntryHeader,
    maybe
      (chunk "")
      ( \ts -> if ts > now then "future" else showDaysSince 21 now ts
      )
      stuckReportEntryLatestChange
  ]
