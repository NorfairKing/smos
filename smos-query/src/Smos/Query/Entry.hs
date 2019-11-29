{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Entry
  ( entry
  ) where

import Control.Arrow

import Conduit

import Smos.Report.Filter
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
  let ees = maybe id sorterSortList entrySetSorter $ map (second forestCursorCurrent) tups
  liftIO $ putTableLn $ renderEntryTable entrySetProjection ees
