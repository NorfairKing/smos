{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Query.Commands.Waiting
  ( waiting
  ) where

import Data.List
import Data.Text (Text)
import Data.Time

import Conduit
import qualified Data.Conduit.Combinators as C
import Rainbow

import Smos.Report.Streaming
import Smos.Report.Waiting

import Smos.Query.Config
import Smos.Query.Formatting
import Smos.Query.OptParse.Types
import Smos.Query.Streaming

waiting :: WaitingSettings -> Q ()
waiting WaitingSettings {..} = do
  tups <-
    sourceToList $
    streamSmosFiles waitingSetHideArchive .| parseSmosFiles .| printShouldPrint PrintWarning .|
    smosFileCursors .|
    smosMFilter waitingSetFilter .|
    smosCursorCurrents .|
    C.filter (isWaitingAction . snd) .|
    C.map (uncurry makeWaitingActionEntry)
  now <- liftIO getCurrentTime
  liftIO $ putTableLn $ renderWaitingActionReport waitingSetThreshold now tups

renderWaitingActionReport :: Word -> UTCTime -> [WaitingActionEntry] -> Table
renderWaitingActionReport threshold now =
  formatAsTable . map (formatWaitingActionEntry threshold now) . sortOn waitingActionEntryTimestamp

formatWaitingActionEntry :: Word -> UTCTime -> WaitingActionEntry -> [Chunk Text]
formatWaitingActionEntry threshold now WaitingActionEntry {..} =
  [ rootedPathChunk waitingActionEntryFilePath
  , headerChunk waitingActionEntryHeader
  , maybe (chunk "") (showDaysSince threshold now) waitingActionEntryTimestamp
  ]
