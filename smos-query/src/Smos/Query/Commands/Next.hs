{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Query.Commands.Next
  ( smosQueryNext
  ) where

import Data.Text (Text)

import Conduit
import qualified Data.Conduit.Combinators as C
import Rainbow

import Smos.Report.Next
import Smos.Report.Streaming

import Smos.Query.Config
import Smos.Query.Formatting
import Smos.Query.OptParse.Types
import Smos.Query.Streaming

smosQueryNext :: NextSettings -> Q ()
smosQueryNext NextSettings {..} = do
  tups <-
    sourceToList $
    streamSmosFiles nextSetHideArchive .| parseSmosFiles .| printShouldPrint PrintWarning .|
    smosFileCursors .|
    smosMFilter nextSetFilter .|
    smosCursorCurrents .|
    C.filter (isNextAction . snd)
  liftIO $ putTableLn $ renderNextActionReport $ makeNextActionReport tups

renderNextActionReport :: NextActionReport -> Table
renderNextActionReport = formatAsTable . map formatNextActionEntry . nextActionReportEntries

formatNextActionEntry :: NextActionEntry -> [Chunk Text]
formatNextActionEntry NextActionEntry {..} =
  [ rootedPathChunk nextActionEntryFilePath
  , maybe (chunk "") todoStateChunk nextActionEntryTodoState
  , headerChunk nextActionEntryHeader
  ]
