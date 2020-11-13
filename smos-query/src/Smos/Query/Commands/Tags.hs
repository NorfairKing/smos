{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Commands.Tags
  ( smosQueryTags,
  )
where

import Conduit
import qualified Data.Conduit.Combinators as C
import Data.List
import qualified Data.Map as M
import Data.Ord
import Rainbow
import Smos.Data
import Smos.Query.Config
import Smos.Query.Formatting
import Smos.Query.OptParse.Types
import Smos.Query.Streaming
import Smos.Report.Streaming
import Smos.Report.Tags

smosQueryTags :: TagsSettings -> Q ()
smosQueryTags TagsSettings {..} = do
  es <-
    sourceToList $
      streamSmosFiles HideArchive
        .| streamParseSmosFiles
        .| smosFileCursors
        .| smosMFilter tagsSetFilter
        .| smosCursorCurrents
        .| C.map snd
  let tr = makeTagsReport es
  liftIO $ putTableLn $ renderTagsReport tr

renderTagsReport :: TagsReport -> Table
renderTagsReport TagsReport {..} =
  formatAsTable $ map (uncurry go) $ sortOn (Down . snd) $ M.toList tagsReportMap
  where
    go :: Tag -> Int -> [Chunk]
    go t n = [tagChunk t, intChunk n]
