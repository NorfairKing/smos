{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Logbook.Gen where

import Cursor.Simple.List.NonEmpty
import Cursor.Simple.List.NonEmpty.Gen ()
import Cursor.Types
import Data.GenValidity
import Smos.Cursor.Logbook
import Smos.Data.Gen ()

instance GenUnchecked LogbookCursor where
  shrinkUnchecked (LogbookCursorClosed Nothing) = []
  shrinkUnchecked (LogbookCursorClosed (Just ne)) =
    [ LogbookCursorClosed $
        case nonEmptyCursorDeleteElem ne of
          Deleted -> Nothing
          Updated ne' -> Just ne'
    ]
  shrinkUnchecked (LogbookCursorOpen _ mne) =
    LogbookCursorClosed mne : shrinkUnchecked (LogbookCursorClosed mne)

instance GenValid LogbookCursor where
  genValid = genValidStructurally
