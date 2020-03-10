{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Commands.Entry
  ( smosQueryEntry
  ) where

import Conduit

import Smos.Report.Entry
import Smos.Report.Sorter
import Smos.Report.Streaming

import Smos.Query.Config
import Smos.Query.Formatting
import Smos.Query.OptParse.Types
import Smos.Query.Streaming

smosQueryEntry :: EntrySettings -> Q ()
smosQueryEntry EntrySettings {..} = do
  tups <-
    sourceToList $
    streamSmosFiles entrySetHideArchive .| parseSmosFiles .| printShouldPrint PrintWarning .|
    smosFileCursors .|
    smosMFilter entrySetFilter
  liftIO $
    putTableLn $
    renderEntryReport $
    makeEntryReport entrySetProjection $ maybe id sorterSortCursorList entrySetSorter tups
