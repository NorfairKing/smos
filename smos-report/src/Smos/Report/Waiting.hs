{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Waiting where

import GHC.Generics

import Data.Time

import Smos.Data

import Smos.Report.Path

isWaitingAction :: Entry -> Bool
isWaitingAction entry = entryState entry == Just "WAITING"

data WaitingActionEntry = WaitingActionEntry
    { waitingActionEntryHeader :: Header
    , waitingActionEntryTimestamp :: Maybe UTCTime
    , waitingActionEntryFilePath :: RootedPath
    } deriving (Show, Eq, Generic)

makeWaitingActionEntry :: RootedPath -> Entry -> WaitingActionEntry
makeWaitingActionEntry rp Entry {..} =
    let time =
            case unStateHistory entryStateHistory of
                [] -> Nothing
                x:_ -> Just $ stateHistoryEntryTimestamp x
     in WaitingActionEntry
            { waitingActionEntryHeader = entryHeader
            , waitingActionEntryTimestamp = time
            , waitingActionEntryFilePath = rp
            }
