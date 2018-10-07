{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Query.Next where

import qualified Data.Text as T
import Data.Text (Text)

import Conduit
import qualified Data.Conduit.Combinators as C
import Path
import Rainbow

import Smos.Report.Next
import Smos.Report.Streaming

import Smos.Query.Formatting
import Smos.Query.OptParse.Types

next :: Settings -> IO ()
next Settings {..} = do
    tups <-
        sourceToList $
        sourceFilesInNonHiddenDirsRecursively setWorkDir .| filterSmosFiles .|
        parseSmosFiles setWorkDir .|
        printShouldPrint setShouldPrint .|
        smosFileEntries .|
        C.filter (isNextAction . snd) .|
        C.map (uncurry makeNextActionEntry)
    putTableLn $ renderNextActionReport tups

renderNextActionReport :: [NextActionEntry] -> Table
renderNextActionReport = formatAsTable . map formatNextActionEntry

formatNextActionEntry :: NextActionEntry -> [Chunk Text]
formatNextActionEntry NextActionEntry {..} =
    [ chunk $ T.pack $ fromRelFile nextActionEntryFilePath
    , maybe (chunk "") todoStateChunk nextActionEntryTodoState
    , headerChunk nextActionEntryHeader
    ]
