{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Query.Entry
  ( entry
  ) where

import Data.List

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
  let sortIt =
        maybe
          id
          (\s -> sortBy $ \(rpa, fca) (rpb, fcb) -> sorterOrdering s rpa fca rpb fcb)
          entrySetSorter
  let ees = sortIt $ tups
  liftIO $ putTableLn $ renderEntryTable entrySetProjection $ map (second forestCursorCurrent) ees
