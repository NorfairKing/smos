{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Waiting
    ( waiting
    ) where

import GHC.Generics

import qualified Data.Text as T
import Data.Text (Text)
import Data.Time

import Conduit
import qualified Data.Conduit.Combinators as C
import Path
import Rainbow

import Smos.Data
import Smos.Report.Formatting
import Smos.Report.OptParse
import Smos.Report.Streaming

waiting :: Settings -> IO ()
waiting Settings {..} = do
    tups <-
        sourceToList $
        sourceFilesInNonHiddenDirsRecursively setWorkDir .| filterSmosFiles .|
        parseSmosFiles setWorkDir .|
        printShouldPrint setShouldPrint .|
        smosFileEntries .|
        C.filter (isWaitingAction . snd) .|
        C.map (uncurry makeWaitingActionEntry)
    now <- getCurrentTime
    putTableLn $ renderWaitingActionReport now tups

isWaitingAction :: Entry -> Bool
isWaitingAction entry = entryState entry == Just "WAITING"

renderWaitingActionReport :: UTCTime -> [WaitingActionEntry] -> Table
renderWaitingActionReport now =
    formatAsTable . map (formatWaitingActionEntry now)

data WaitingActionEntry = WaitingActionEntry
    { waitingActionEntryHeader :: Header
    , waitingActionEntryTimestamp :: Maybe UTCTime
    , waitingActionEntryFilePath :: Path Rel File
    } deriving (Show, Eq, Generic)

makeWaitingActionEntry :: Path Rel File -> Entry -> WaitingActionEntry
makeWaitingActionEntry rf Entry {..} =
    let time =
            case unStateHistory entryStateHistory of
                [] -> Nothing
                x:_ -> Just $ stateHistoryEntryTimestamp x
     in WaitingActionEntry
            { waitingActionEntryHeader = entryHeader
            , waitingActionEntryTimestamp = time
            , waitingActionEntryFilePath = rf
            }

formatWaitingActionEntry :: UTCTime -> WaitingActionEntry -> [Chunk Text]
formatWaitingActionEntry now WaitingActionEntry {..} =
    [ chunk $ T.pack $ fromRelFile waitingActionEntryFilePath
    , headerChunk $ waitingActionEntryHeader
    , maybe (chunk "") (showDaysSince now) waitingActionEntryTimestamp
    ]

showDaysSince :: UTCTime -> UTCTime -> Chunk Text
showDaysSince now t = fore color $ chunk $ T.pack $ show i <> " days"
  where
    color
        | i > 21 = red
        | i > 15 = yellow
        | i > 5 = blue
        | otherwise = mempty
    i = diffInDays now t :: Int
    diffInDays :: UTCTime -> UTCTime -> Int
    diffInDays t1 t2 = floor $ diffUTCTime t1 t2 / nominalDay
