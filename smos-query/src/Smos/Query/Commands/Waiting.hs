{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Commands.Waiting
  ( smosQueryWaiting,
  )
where

import Conduit
import Data.Time
import Rainbow
import Smos.Query.Config
import Smos.Query.Formatting
import Smos.Query.OptParse.Types
import Smos.Report.Waiting

smosQueryWaiting :: WaitingSettings -> Q ()
smosQueryWaiting WaitingSettings {..} = do
  dc <- asks $ smosReportConfigDirectoryConfig . smosQueryConfigReportConfig
  report <- produceWaitingReport waitingSetFilter waitingSetHideArchive dc
  now <- liftIO getCurrentTime
  liftIO $ putTableLn $ renderWaitingReport waitingSetThreshold now report

renderWaitingReport :: Word -> UTCTime -> WaitingReport -> Table
renderWaitingReport threshold now =
  formatAsTable . map (formatWaitingEntry threshold now) . waitingReportEntries

formatWaitingEntry :: Word -> UTCTime -> WaitingEntry -> [Chunk]
formatWaitingEntry threshold now WaitingEntry {..} =
  [ pathChunk waitingEntryFilePath,
    headerChunk waitingEntryHeader,
    showDaysSince threshold now waitingEntryTimestamp
  ]
