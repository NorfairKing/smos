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

import Smos.Query.Config
import Smos.Query.Formatting
import Smos.Query.OptParse.Types

next :: Q ()
next = do
    wd <- askWorkDir
    liftIO $ do
        tups <-
            sourceToList $
            sourceFilesInNonHiddenDirsRecursively wd .| filterSmosFiles .|
            parseSmosFiles wd .|
            printShouldPrint PrintWarning .|
            smosFileEntries .|
            C.filter (isNextAction . snd) .|
            C.map (uncurry makeNextActionEntry)
        putTableLn $ renderNextActionReport tups

renderNextActionReport :: [NextActionEntry] -> Table
renderNextActionReport = formatAsTable . map formatNextActionEntry

formatNextActionEntry :: NextActionEntry -> [Chunk Text]
formatNextActionEntry NextActionEntry {..} =
    [ rootedPathChunk nextActionEntryFilePath
    , maybe (chunk "") todoStateChunk nextActionEntryTodoState
    , headerChunk nextActionEntryHeader
    ]
