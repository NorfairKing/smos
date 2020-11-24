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
  putTableLn $ renderWaitingReport waitingSetThreshold now report

renderWaitingReport :: Word -> UTCTime -> WaitingReport -> Table
renderWaitingReport threshold now =
  formatAsTable . map (formatWaitingEntry threshold now) . waitingReportEntries
