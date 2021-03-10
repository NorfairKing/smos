{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Commands.Stuck where

import Conduit
import qualified Data.Conduit.List as C
import Data.Time
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
    fmap makeStuckReport $
      sourceToList $
        streamSmosProjectsQ
          .| smosMFilter (FilterFst <$> stuckSetFilter)
          .| C.map (uncurry (makeStuckReportEntry (zonedTimeZone now)))
          .| C.catMaybes
  liftIO $ putChunks $ renderStuckReport stuckSetThreshold (zonedTimeToUTC now) stuckReport

renderStuckReport :: Word -> UTCTime -> StuckReport -> [Chunk]
renderStuckReport threshold now = formatAsBicolourTable . map (formatStuckReportEntry threshold now) . stuckReportEntries
