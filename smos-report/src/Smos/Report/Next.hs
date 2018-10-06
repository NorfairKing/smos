{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Next where

import Data.Maybe

import qualified Data.Text as T
import Data.Text (Text)

import Conduit
import qualified Data.Conduit.Combinators as C
import Path
import Rainbow

import Smos.Data

import Smos.Report.Formatting
import Smos.Report.OptParse
import Smos.Report.Streaming

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

isNextAction :: Entry -> Bool
isNextAction entry =
    or $
    (==) (entryState entry) . Just <$> mapMaybe todoState ["NEXT", "STARTED"]

makeNextActionEntry :: Path Rel File -> Entry -> NextActionEntry
makeNextActionEntry rf e =
    NextActionEntry
        { nextActionEntryTodoState = entryState e
        , nextActionEntryHeader = entryHeader e
        , nextActionEntryFilePath = rf
        }

renderNextActionReport :: [NextActionEntry] -> Table
renderNextActionReport = formatAsTable . map formatNextActionEntry

data NextActionEntry = NextActionEntry
    { nextActionEntryTodoState :: Maybe TodoState
    , nextActionEntryHeader :: Header
    , nextActionEntryFilePath :: Path Rel File
    } deriving (Show, Eq)

formatNextActionEntry :: NextActionEntry -> [Chunk Text]
formatNextActionEntry NextActionEntry {..} =
    [ chunk $ T.pack $ fromRelFile nextActionEntryFilePath
    , maybe (chunk "") todoStateChunk nextActionEntryTodoState
    , headerChunk nextActionEntryHeader
    ]
