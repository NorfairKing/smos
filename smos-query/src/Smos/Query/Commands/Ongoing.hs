{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Commands.Ongoing (smosQueryOngoing) where

import Conduit
import Smos.Query.Commands.Import
import Smos.Report.Ongoing

smosQueryOngoing :: OngoingSettings -> Q ()
smosQueryOngoing OngoingSettings {..} = do
  dc <- asks envDirectorySettings
  sp <- getShouldPrint
  zone <- liftIO loadLocalTZ
  now <- liftIO getCurrentTime
  report <- liftIO $ produceOngoingReport zone now ongoingSetFilter ongoingSetHideArchive sp dc

  colourSettings <- asks envColourSettings
  outputChunks $ renderOngoingReport zone now colourSettings report

renderOngoingReport :: TZ -> UTCTime -> ColourSettings -> OngoingReport -> [Chunk]
renderOngoingReport zone now colourSettings =
  formatAsBicolourTable colourSettings
    . map (formatOngoingEntry zone now)
    . ongoingReportEntries
