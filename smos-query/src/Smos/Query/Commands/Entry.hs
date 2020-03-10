{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Commands.Entry
  ( entry
  ) where

import Conduit

import Smos.Report.Sorter
import Smos.Report.Streaming

import Smos.Query.Config
import Smos.Query.Formatting
import Smos.Query.OptParse.Types
import Smos.Query.Streaming

entry :: EntrySettings -> Q ()
entry EntrySettings {..} = do
  tups <-
    sourceToList $
    streamSmosFiles entrySetHideArchive .| parseSmosFiles .| printShouldPrint PrintWarning .|
    smosFileCursors .|
    smosMFilter entrySetFilter
  let ees = maybe id sorterSortCursorList entrySetSorter tups
  liftIO $ putTableLn $ renderEntryTable entrySetProjection ees
