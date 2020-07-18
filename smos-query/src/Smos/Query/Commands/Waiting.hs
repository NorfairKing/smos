{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Commands.Waiting
  ( smosQueryWaiting,
  )
where

import Conduit
import qualified Data.Conduit.Combinators as C
import Data.Time
import Rainbow
import Smos.Query.Config
import Smos.Query.Formatting
import Smos.Query.OptParse.Types
import Smos.Query.Streaming
import Smos.Report.Streaming
import Smos.Report.Waiting

smosQueryWaiting :: WaitingSettings -> Q ()
smosQueryWaiting WaitingSettings {..} = do
  tups <-
    sourceToList $
      streamSmosFiles waitingSetHideArchive .| parseSmosFiles .| printShouldPrint PrintWarning
        .| smosFileCursors
        .| smosMFilter waitingSetFilter
        .| smosCursorCurrents
        .| C.filter (isWaitingAction . snd)
  now <- liftIO getCurrentTime
  liftIO $ putTableLn $ renderWaitingActionReport waitingSetThreshold now $ makeWaitingReport tups

renderWaitingActionReport :: Word -> UTCTime -> WaitingReport -> Table
renderWaitingActionReport threshold now =
  formatAsTable . map (formatWaitingActionEntry threshold now) . waitingReportEntries

formatWaitingActionEntry :: Word -> UTCTime -> WaitingActionEntry -> [Chunk]
formatWaitingActionEntry threshold now WaitingActionEntry {..} =
  [ rootedPathChunk waitingActionEntryFilePath,
    headerChunk waitingActionEntryHeader,
    maybe (chunk "") (showDaysSince threshold now) waitingActionEntryTimestamp
  ]
