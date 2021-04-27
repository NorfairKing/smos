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
  cc <- asks smosQueryConfigColourConfig

  outputChunks $ renderWaitingReport cc waitingSetThreshold now report

renderWaitingReport :: ColourConfig -> Word -> UTCTime -> WaitingReport -> [Chunk]
renderWaitingReport cc threshold now =
  formatAsBicolourTable cc . map (formatWaitingEntry threshold now) . waitingReportEntries
