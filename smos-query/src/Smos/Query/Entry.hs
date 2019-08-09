{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Query.Entry
  ( entry
  ) where

import Data.List
import Data.Maybe
import Data.Text (Text)

import Conduit
import qualified Data.Conduit.Combinators as C
import Rainbow

import Cursor.Simple.Forest

import Smos.Data

import Smos.Report.Filter
import Smos.Report.Path
import Smos.Report.Projection
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
  let defaultProjection = foldl1 AndAlso [OntoFile, OntoState, OntoHeader]
  let projection = fromMaybe defaultProjection entrySetProjection
  liftIO $ putTableLn $ renderEntryReport projection ees

renderEntryReport :: Projection -> [(RootedPath, ForestCursor Entry)] -> Table
renderEntryReport projection =
  formatAsTable .
  (\l ->
     if null l
       then []
       else renderProjectionHeader projection : l) .
  map renderProjectees . map (uncurry (performProjection projection))

renderProjectionHeader :: Projection -> [Chunk Text]
renderProjectionHeader p =
  case p of
    OntoFile -> [chunk "file"]
    OntoHeader -> [chunk "header"]
    OntoProperty pn -> [chunk $ propertyNameText pn]
    OntoState -> [chunk "state"]
    AndAlso p1 p2 -> renderProjectionHeader p1 ++ renderProjectionHeader p2

renderProjectees :: [Projectee] -> [Chunk Text]
renderProjectees = map projecteeChunk

projecteeChunk :: Projectee -> Chunk Text
projecteeChunk p =
  case p of
    FileProjection rp -> rootedPathChunk rp
    HeaderProjection h -> headerChunk h
    StateProjection s -> maybe (chunk "") todoStateChunk s
    PropertyProjection pn pv -> maybe (chunk "") (propertyValueChunk pn) pv
