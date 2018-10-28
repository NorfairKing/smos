{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Query.Entry
    ( entry
    ) where

import Data.Text (Text)

import Conduit
import qualified Data.Conduit.Combinators as C
import Rainbow

import Smos.Data

import Smos.Report.Entry
import Smos.Report.Query
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
            C.filter (maybe (const True) filterPredicate entrySetFilter . snd) .|
            smosCursorCurrents .|
            C.map (uncurry makeEntryEntry)
        putTableLn $ renderEntryReport tups

renderEntryReport :: [EntryEntry] -> Table
renderEntryReport = formatAsTable . map renderEntryEntry

renderEntryEntry :: EntryEntry -> [Chunk Text]
renderEntryEntry EntryEntry {..} =
    let e@Entry {..} = entryEntryEntry
     in [ rootedPathChunk entryEntryFilePath
        , maybe (chunk "") todoStateChunk $ entryState e
        , headerChunk entryHeader
        ]
