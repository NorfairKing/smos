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

formatWaitingActionEntry :: UTCTime -> WaitingActionEntry -> [Text]
formatWaitingActionEntry now WaitingActionEntry {..} =
    [ T.pack $ fromRelFile waitingActionEntryFilePath
    , headerText $ waitingActionEntryHeader
    , maybe "" (T.pack . showDaysSince now) waitingActionEntryTimestamp
    ]

showDaysSince :: UTCTime -> UTCTime -> String
showDaysSince now t = show (diffInDays now t :: Int) <> " days"
  where
    diffInDays t1 t2 = floor $ diffUTCTime t1 t2 / nominalDay
