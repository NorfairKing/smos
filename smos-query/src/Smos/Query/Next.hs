{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Query.Next where

import Data.Text (Text)

import Conduit
import qualified Data.Conduit.Combinators as C
import Rainbow

import Smos.Report.Next
import Smos.Report.Query
import Smos.Report.Streaming

import Smos.Query.Config
import Smos.Query.Formatting
import Smos.Query.OptParse.Types

next :: NextSettings -> Q ()
next NextSettings {..} = do
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
                     maybe
                         True
                         (\f -> filterPredicate f rp fc)
                         nextSetFilter) .|
            smosCursorCurrents .|
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
