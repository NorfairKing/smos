{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Next where

import Data.Maybe

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T

import Conduit
import qualified Data.Conduit.Combinators as C
import Path

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
    T.putStr $ renderNextActionReport tups

isNextAction :: Entry -> Bool
isNextAction entry =
    or $
    (==) (entryState entry) . Just <$>
    mapMaybe todoState ["WAITING", "NEXT", "STARTED", "READY"]

makeNextActionEntry :: Path Rel File -> Entry -> NextActionEntry
makeNextActionEntry rf e =
    NextActionEntry
        { nextActionEntryTodoState = entryState e
        , nextActionEntryHeader = entryHeader e
        , nextActionEntryFilePath = rf
        }

renderNextActionReport :: [NextActionEntry] -> Text
renderNextActionReport = T.pack . formatAsTable . map formatNextActionEntry

data NextActionEntry = NextActionEntry
    { nextActionEntryTodoState :: Maybe TodoState
    , nextActionEntryHeader :: Header
    , nextActionEntryFilePath :: Path Rel File
    } deriving (Show, Eq)

formatNextActionEntry :: NextActionEntry -> [String]
formatNextActionEntry NextActionEntry {..} =
    [ fromRelFile nextActionEntryFilePath
    , maybe "" (T.unpack . todoStateText) nextActionEntryTodoState
    , T.unpack $ headerText nextActionEntryHeader
    ]
