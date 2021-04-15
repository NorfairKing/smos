{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Commands.Entry
  ( smosQueryEntry,
  )
where

import Smos.Query.Config
import Smos.Query.Formatting
import Smos.Query.OptParse.Types
import Smos.Report.Entry

smosQueryEntry :: EntrySettings -> Q ()
smosQueryEntry EntrySettings {..} = do
  dc <- asks $ smosReportConfigDirectoryConfig . smosQueryConfigReportConfig
  sp <- getShouldPrint
  report <- produceEntryReport entrySetFilter entrySetHideArchive sp entrySetProjection entrySetSorter dc
  cc <- asks smosQueryConfigColourConfig
  outputChunks $ renderEntryReport cc report
