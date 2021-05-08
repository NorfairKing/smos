{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Commands.Waiting
  ( smosQueryWaiting,
  )
where

import Conduit
import Smos.Query.Commands.Import
import Smos.Report.Time
import Smos.Report.Waiting

smosQueryWaiting :: WaitingSettings -> Q ()
smosQueryWaiting WaitingSettings {..} = do
  dc <- asks envDirectoryConfig
  sp <- getShouldPrint
  report <- produceWaitingReport waitingSetFilter waitingSetHideArchive sp dc

  now <- liftIO getCurrentTime
  colourSettings <- asks envColourSettings

  outputChunks $ renderWaitingReport colourSettings waitingSetThreshold now report

renderWaitingReport :: ColourSettings -> Time -> UTCTime -> WaitingReport -> [Chunk]
renderWaitingReport colourSettings threshold now WaitingReport {..} =
  formatAsBicolourTable colourSettings $
    map underline [chunk "file", headerChunk "header", fore blue $ chunk "waiting", chunk "threshold"] :
    map (formatWaitingEntry threshold now) waitingReportEntries
