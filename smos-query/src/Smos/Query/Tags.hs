{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Query.Tags
  ( tags
  ) where

import qualified Data.Map as M
import Data.Text (Text)

import Conduit
import qualified Data.Conduit.Combinators as C
import Rainbow


import Smos.Data

import Smos.Report.Filter
import Smos.Report.Streaming
import Smos.Report.Tags

import Smos.Query.Config
import Smos.Query.Formatting
import Smos.Query.OptParse.Types
import Smos.Query.Streaming

tags :: TagsSettings -> Q ()
tags TagsSettings {..} = do
  es <-
    sourceToList $
    streamSmosFiles .| parseSmosFiles .| printShouldPrint PrintWarning .| smosFileCursors .|
    C.filter (\(rp, fc) -> maybe True (\f -> filterPredicate f rp fc) tagsSetFilter) .|
    smosCursorCurrents .|
    C.map snd
  let tr = makeTagsReport es
  liftIO $ putTableLn $ renderTagsReport tr

renderTagsReport :: TagsReport -> Table
renderTagsReport TagsReport {..} = formatAsTable $ map (uncurry go) $ M.toAscList tagsReportMap
  where
    go :: Tag -> Int -> [Chunk Text]
    go t n = [tagChunk t, intChunk n]
