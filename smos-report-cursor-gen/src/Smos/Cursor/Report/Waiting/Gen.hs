{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Report.Waiting.Gen where

import Cursor.Forest.Gen ()
import Cursor.Text.Gen ()
import Data.GenValidity
import Data.GenValidity.Path ()
import Smos.Cursor.Report.Entry.Gen
import Smos.Cursor.Report.Waiting
import Smos.Data
import Smos.Data.Gen ()
import Smos.Report.Waiting
import Test.QuickCheck

instance GenValid WaitingReportCursor where
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
  genValid = WaitingReportCursor <$> genValidEntryReportCursorWith makeWaitingEntryCursor' sortWaitingReport genWaitingEntry

genWaitingEntry :: Gen Entry
genWaitingEntry = do
  timestamp <- genValid
  let modStateHistory (StateHistory shes) = StateHistory $ case shes of
        [] ->
          [ StateHistoryEntry
              { stateHistoryEntryNewState = Just waitingState,
                stateHistoryEntryTimestamp = timestamp
              }
          ]
        (she : rest) -> she {stateHistoryEntryNewState = Just waitingState} : rest
  e <- genValid
  pure $
    e
      { entryStateHistory = modStateHistory (entryStateHistory e)
      }
