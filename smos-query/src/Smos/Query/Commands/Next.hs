{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Commands.Next
  ( smosQueryNext,
  )
where

import Conduit
import Data.Text (Text)
import Rainbow
import Smos.Query.Config
import Smos.Query.Formatting
import Smos.Query.OptParse.Types
import Smos.Report.Next

smosQueryNext :: NextSettings -> Q ()
smosQueryNext NextSettings {..} = do
  dc <- asks $ smosReportConfigDirectoryConfig . smosQueryConfigReportConfig
  liftIO $ do
    report <- produceNextActionReport nextSetFilter nextSetHideArchive dc
    putTableLn $ renderNextActionReport report

renderNextActionReport :: NextActionReport -> Table
renderNextActionReport = formatAsTable . map formatNextActionEntry . nextActionReportEntries

formatNextActionEntry :: NextActionEntry -> [Chunk Text]
formatNextActionEntry NextActionEntry {..} =
  [ pathChunk nextActionEntryFilePath,
    maybe (chunk "") todoStateChunk nextActionEntryTodoState,
    headerChunk nextActionEntryHeader
  ]
