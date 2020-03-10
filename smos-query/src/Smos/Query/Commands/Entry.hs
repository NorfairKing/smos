{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Commands.Entry
  ( smosQueryEntry
  ) where

import Conduit

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
  let ees = maybe id sorterSortCursorList entrySetSorter tups
  liftIO $ putTableLn $ renderEntryTable entrySetProjection ees
