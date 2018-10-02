{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Waiting
    ( waiting
    ) where

import GHC.Generics


import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T
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
        sourceNonhiddenFiles setWorkDir .| filterSmosFiles .|
        parseSmosFiles setWorkDir setShouldPrint .|
        smosFileEntries .|
        C.filter (isWaitingAction . snd) .|
        C.map (uncurry makeWaitingActionEntry)
    now <- getCurrentTime
    T.putStr $ renderWaitingActionReport now tups

isWaitingAction :: Entry -> Bool
isWaitingAction entry = entryState entry == Just "WAITING"

renderWaitingActionReport :: UTCTime -> [WaitingActionEntry] -> Text
renderWaitingActionReport now =
    T.pack . formatAsTable . map (formatWaitingActionEntry now)

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

formatWaitingActionEntry :: UTCTime -> WaitingActionEntry -> [String]
formatWaitingActionEntry now WaitingActionEntry {..} =
    [ fromRelFile waitingActionEntryFilePath
    , T.unpack $ headerText $ waitingActionEntryHeader
    , maybe "" (showDaysSince now) waitingActionEntryTimestamp
    ]

showDaysSince :: UTCTime -> UTCTime -> String
showDaysSince now t = show (diffInDays now t :: Int) <> " days"
  where
    diffInDays t1 t2 = floor $ diffUTCTime t1 t2 / nominalDay
