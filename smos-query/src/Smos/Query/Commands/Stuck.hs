{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Commands.Stuck where

import Conduit
import qualified Data.Conduit.List as C
import Smos.Query.Commands.Import
import Smos.Report.Stuck
import Smos.Report.Time

smosQueryStuck :: StuckSettings -> Q ()
smosQueryStuck StuckSettings {..} = do
  zone <- liftIO loadLocalTZ
  now <- liftIO getCurrentTime
  stuckReport <-
    fmap makeStuckReport $
      sourceToList $
        streamSmosProjectsQ
          .| smosMFilter (FilterFst <$> stuckSetFilter)
          .| C.map (uncurry (makeStuckReportEntry zone))
          .| C.catMaybes
  colourSettings <- asks envColourSettings
  outputChunks $ renderStuckReport colourSettings stuckSetThreshold now stuckReport

renderStuckReport :: ColourSettings -> Time -> UTCTime -> StuckReport -> [Chunk]
renderStuckReport colourSettings threshold now =
  formatAsBicolourTable colourSettings
    . map (formatStuckReportEntry threshold now)
    . stuckReportEntries
