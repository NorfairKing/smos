{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Query.Entry
  ( entry
  ) where

import Control.Arrow

import Conduit
import qualified Data.Conduit.Combinators as C

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
    C.filter (\(rp, fc) -> maybe True (\f -> filterPredicate f rp fc) entrySetFilter)
  let ees = maybe id sorterSortList entrySetSorter $ map (second forestCursorCurrent) tups
  liftIO $ putTableLn $ renderEntryTable entrySetProjection ees
