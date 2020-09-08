{-# LANGUAGE DeriveGeneric #-}

module Smos.Cursor.Report.Waiting where

import Cursor.Simple.Forest
import Cursor.Simple.List.NonEmpty
import Data.Validity
import Data.Validity.Path ()
import GHC.Generics
import Lens.Micro
import Path
import Smos.Data

data WaitingReportCursor
  = WaitingReportCursor
      { waitingReportCursorWaitingEntryCursors :: Maybe (NonEmptyCursor WaitingEntryCursor)
      }
  deriving (Show, Eq, Generic)

instance Validity WaitingReportCursor

data WaitingEntryCursor
  = WaitingEntryCursor
      { waitingEntryCursorFilePath :: Path Rel File,
        waitingEntryCursorForestCursor :: ForestCursor Entry
      }
  deriving (Show, Eq, Generic)

instance Validity WaitingEntryCursor

waitingReportCursorNonEmptyCursorL :: Lens' WaitingReportCursor (Maybe (NonEmptyCursor WaitingEntryCursor))
waitingReportCursorNonEmptyCursorL = lens waitingReportCursorWaitingEntryCursors $ \wrc ne -> wrc {waitingReportCursorWaitingEntryCursors = ne}
