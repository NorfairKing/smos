{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Waiting where

import GHC.Generics

import Data.Time

import Path

import Smos.Data

isWaitingAction :: Entry -> Bool
isWaitingAction entry = entryState entry == Just "WAITING"

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
