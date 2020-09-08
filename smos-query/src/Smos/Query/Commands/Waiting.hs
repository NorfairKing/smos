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
  liftIO $ putTableLn $ renderWaitingActionReport waitingSetThreshold now report

renderWaitingActionReport :: Word -> UTCTime -> WaitingReport -> Table
renderWaitingActionReport threshold now =
  formatAsTable . map (formatWaitingActionEntry threshold now) . waitingReportEntries

formatWaitingActionEntry :: Word -> UTCTime -> WaitingActionEntry -> [Chunk]
formatWaitingActionEntry threshold now WaitingActionEntry {..} =
  [ pathChunk waitingActionEntryFilePath,
    headerChunk waitingActionEntryHeader,
    maybe (chunk "") (showDaysSince threshold now) waitingActionEntryTimestamp
  ]
