{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Commands.Next
  ( smosQueryNext,
  )
where

import Conduit
import Smos.Query.Commands.Import
import Smos.Report.Next

smosQueryNext :: NextSettings -> Q ()
smosQueryNext NextSettings {..} = do
  dc <- asks envDirectorySettings
  sp <- getShouldPrint
  report <- liftIO $ produceNextActionReport nextSetFilter nextSetHideArchive sp dc
  colourSettings <- asks envColourSettings
  outputChunks $ renderNextActionReport colourSettings report

renderNextActionReport :: ColourSettings -> NextActionReport -> [Chunk]
renderNextActionReport colourSettings = formatAsBicolourTable colourSettings . map formatNextActionEntry . nextActionReportEntries

formatNextActionEntry :: NextActionEntry -> [Chunk]
formatNextActionEntry NextActionEntry {..} =
  [ pathChunk nextActionEntryFilePath,
    mTodoStateChunk nextActionEntryTodoState,
    headerChunk nextActionEntryHeader
  ]
