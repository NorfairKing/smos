{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Query.Entry
    ( entry
    ) where

import Data.List
import Data.Text (Text)

import Conduit
import qualified Data.Conduit.Combinators as C
import Rainbow

import Smos.Data

import Smos.Report.Entry
import Smos.Report.Query
import Smos.Report.Sorter
import Smos.Report.Streaming

import Smos.Query.Config
import Smos.Query.Formatting
import Smos.Query.OptParse.Types

entry :: EntrySettings -> Q ()
entry EntrySettings {..} = do
    wd <- askWorkDir
    liftIO $ do
        tups <-
            sourceToList $
            sourceFilesInNonHiddenDirsRecursively wd .| filterSmosFiles .|
            parseSmosFiles .|
            printShouldPrint PrintWarning .|
            smosFileCursors .|
            C.filter
                (\(rp, fc) ->
                     maybe True (\f -> filterPredicate f rp fc) entrySetFilter)
            -- smosCursorCurrents .|
            -- C.map (uncurry makeEntryEntry)
        let sortIt =
                maybe
                    id
                    (\s ->
                         sortBy $ \(rpa, fca) (rpb, fcb) ->
                             sorterOrdering s rpa fca rpb fcb)
                    entrySetSorter
        let ees =
                map (uncurry makeEntryEntry . smosCursorCurrent) . sortIt $ tups
        putTableLn $ renderEntryReport ees

renderEntryReport :: [EntryEntry] -> Table
renderEntryReport = formatAsTable . map renderEntryEntry

renderEntryEntry :: EntryEntry -> [Chunk Text]
renderEntryEntry EntryEntry {..} =
    let e@Entry {..} = entryEntryEntry
     in [ rootedPathChunk entryEntryFilePath
        , maybe (chunk "") todoStateChunk $ entryState e
        , headerChunk entryHeader
        ]
