{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Commands.Stuck where

import Conduit
import qualified Data.Conduit.List as C
import Smos.Query.Commands.Import
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
  colourSettings <- asks envColourSettings
  outputChunks $ renderStuckReport colourSettings stuckSetThreshold (zonedTimeToUTC now) stuckReport

renderStuckReport :: ColourSettings -> Word -> UTCTime -> StuckReport -> [Chunk]
renderStuckReport colourSettings threshold now =
  formatAsBicolourTable colourSettings
    . map (formatStuckReportEntry threshold now)
    . stuckReportEntries
