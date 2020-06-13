{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Commands.Entry
  ( smosQueryEntry,
  )
where

import Conduit
import Smos.Query.Config
import Smos.Query.Formatting
import Smos.Query.OptParse.Types
import Smos.Report.Entry

smosQueryEntry :: EntrySettings -> Q ()
smosQueryEntry EntrySettings {..} = do
  dc <- asks $ smosReportConfigDirectoryConfig . smosQueryConfigReportConfig
  report <- produceEntryReport entrySetFilter entrySetHideArchive entrySetProjection entrySetSorter dc
  liftIO $ putTableLn $ renderEntryReport report
