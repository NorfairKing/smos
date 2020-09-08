{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Report.Waiting.Gen where

import Cursor.Forest
import Cursor.Forest.Gen ()
import Cursor.Text.Gen ()
import Cursor.Tree
import Data.GenValidity
import Data.GenValidity.Path ()
import Debug.Trace
import Lens.Micro
import Smos.Cursor.Report.Waiting
import Smos.Data
import Smos.Data.Gen ()
import Smos.Report.Path.Gen ()
import Smos.Report.Waiting
import Test.QuickCheck

instance GenValid WaitingReportCursor where
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
  genValid = makeWaitingReportCursor <$> genValid

instance GenValid WaitingEntryCursor where
  genValid = do
    path <- genValid
    ( do
        fc <- genValid
        timestamp <- genValid
        let modStateHistory (StateHistory shes) = StateHistory $ case shes of
              [] ->
                [ StateHistoryEntry
                    { stateHistoryEntryNewState = Just waitingState,
                      stateHistoryEntryTimestamp = timestamp
                    }
                ]
              (she : rest) -> she {stateHistoryEntryNewState = Just waitingState} : rest
        pure $
          fc & forestCursorSelectedTreeL . treeCursorCurrentL
            %~ ( \e ->
                   e
                     { entryStateHistory = modStateHistory (entryStateHistory e)
                     }
               )
      )
      `suchThatMap` makeWaitingEntryCursor path
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
