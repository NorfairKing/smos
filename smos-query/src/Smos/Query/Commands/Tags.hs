{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Commands.Tags
  ( smosQueryTags,
  )
where

import Conduit
import qualified Data.Conduit.Combinators as C
import qualified Data.Map as M
import Smos.Query.Commands.Import
import Smos.Report.Tags

smosQueryTags :: TagsSettings -> Q ()
smosQueryTags TagsSettings {..} = do
  es <-
    sourceToList $
      streamSmosFiles HideArchive
        .| streamParseSmosFiles
        .| smosFileCursors
        .| mFilterConduit tagsSetFilter
        .| smosCursorCurrents
        .| C.map snd
  let tagsReport = makeTagsReport es
  colourSettings <- asks envColourSettings
  outputChunks $ renderTagsReport colourSettings tagsReport

renderTagsReport :: ColourSettings -> TagsReport -> [Chunk]
renderTagsReport colourSettings TagsReport {..} =
  formatAsBicolourTable colourSettings $ map (uncurry go) $ sortOn (Down . snd) $ M.toList tagsReportMap
  where
    go :: Tag -> Int -> [Chunk]
    go t n = [tagChunk t, intChunk n]
