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
  putTableLn $ renderStuckReport (zonedTimeToUTC now) stuckSetThreshold stuckReport

renderStuckReport :: UTCTime -> Word -> StuckReport -> Table
renderStuckReport now threshold = formatAsTable . map (renderStuckReportEntry now threshold) . stuckReportEntries

renderStuckReportEntry :: UTCTime -> Word -> StuckReportEntry -> [Chunk]
renderStuckReportEntry now threshold StuckReportEntry {..} =
  [ pathChunk stuckReportEntryFilePath,
    mTodoStateChunk stuckReportEntryState,
    headerChunk stuckReportEntryHeader,
    maybe
      (chunk "")
      ( \ts -> if ts > now then "future" else showDaysSince threshold now ts
      )
      stuckReportEntryLatestChange
  ]
