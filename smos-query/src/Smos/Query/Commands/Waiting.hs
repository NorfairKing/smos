{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Commands.Waiting
  ( smosQueryWaiting,
  )
where

import Conduit
import Data.Time
import Smos.Query.Config
import Smos.Query.Formatting
import Smos.Query.OptParse.Types
import Smos.Report.Waiting

smosQueryWaiting :: WaitingSettings -> Q ()
smosQueryWaiting WaitingSettings {..} = do
  dc <- asks $ smosReportConfigDirectoryConfig . smosQueryConfigReportConfig
  sp <- getShouldPrint
  report <- produceWaitingReport waitingSetFilter waitingSetHideArchive sp dc
  now <- liftIO getCurrentTime
  liftIO $ putChunks $ renderWaitingReport waitingSetThreshold now report

renderWaitingReport :: Word -> UTCTime -> WaitingReport -> [Chunk]
renderWaitingReport threshold now =
  formatAsBicolourTable . map (formatWaitingEntry threshold now) . waitingReportEntries
