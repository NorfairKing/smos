{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Commands.Waiting
  ( smosQueryWaiting,
  )
where

import Conduit
import Smos.Query.Commands.Import
import Smos.Report.Waiting

smosQueryWaiting :: WaitingSettings -> Q ()
smosQueryWaiting WaitingSettings {..} = do
  dc <- asks envDirectoryConfig
  sp <- getShouldPrint
  report <- produceWaitingReport waitingSetFilter waitingSetHideArchive sp dc
  now <- liftIO getCurrentTime
  colourSettings <- asks envColourSettings

  outputChunks $ renderWaitingReport colourSettings waitingSetThreshold now report

renderWaitingReport :: ColourSettings -> Word -> UTCTime -> WaitingReport -> [Chunk]
renderWaitingReport colourSettings threshold now =
  formatAsBicolourTable colourSettings . map (formatWaitingEntry threshold now) . waitingReportEntries
