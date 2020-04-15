{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.Waiting where

import Data.List
import Data.Time
import Data.Validity
import Data.Validity.Time ()
import GHC.Generics
import Smos.Data
import Smos.Report.Path

newtype WaitingReport
  = WaitingReport
      { waitingReportEntries :: [WaitingActionEntry]
      }
  deriving (Show, Eq, Generic)

instance Validity WaitingReport

makeWaitingReport :: [(RootedPath, Entry)] -> WaitingReport
makeWaitingReport =
  WaitingReport . sortOn waitingActionEntryTimestamp . map (uncurry makeWaitingActionEntry)

data WaitingActionEntry
  = WaitingActionEntry
      { waitingActionEntryHeader :: Header,
        waitingActionEntryTimestamp :: Maybe UTCTime,
        waitingActionEntryFilePath :: RootedPath
      }
  deriving (Show, Eq, Generic)

instance Validity WaitingActionEntry

makeWaitingActionEntry :: RootedPath -> Entry -> WaitingActionEntry
makeWaitingActionEntry rp Entry {..} =
  let time =
        case unStateHistory entryStateHistory of
          [] -> Nothing
          x : _ -> Just $ stateHistoryEntryTimestamp x
   in WaitingActionEntry
        { waitingActionEntryHeader = entryHeader,
          waitingActionEntryTimestamp = time,
          waitingActionEntryFilePath = rp
        }

isWaitingAction :: Entry -> Bool
isWaitingAction entry = entryState entry == Just "WAITING"
