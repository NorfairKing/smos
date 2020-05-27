{-# LANGUAGE DeriveGeneric #-}

module Smos.History
  ( History (..),
    historyNonEmptyCursorL,
    historyPresentL,
    startingHistory,
    historyPresent,
    historyPush,
    historyUndo,
    historyRedo,
  )
where

import Control.DeepSeq
import Cursor.Simple.List.NonEmpty
import Data.Validity
import GHC.Generics (Generic)
import Lens.Micro

-- | An simple state-based history for a state 's'
newtype History s
  = History {historyNonEmptyCursor :: NonEmptyCursor s}
  deriving (Show, Eq, Generic)

instance Validity s => Validity (History s)

instance NFData s => NFData (History s)

historyNonEmptyCursorL :: Lens' (History s) (NonEmptyCursor s)
historyNonEmptyCursorL = lens historyNonEmptyCursor $ \h lc -> h {historyNonEmptyCursor = lc}

historyPresentL :: Lens' (History s) s
historyPresentL = historyNonEmptyCursorL . nonEmptyCursorElemL

startingHistory :: s -> History s
startingHistory s = History {historyNonEmptyCursor = singletonNonEmptyCursor s}

historyPresent :: History s -> s
historyPresent = nonEmptyCursorCurrent . historyNonEmptyCursor

historyPush :: s -> History s -> History s
historyPush s h =
  h
    { historyNonEmptyCursor =
        (nonEmptyCursorInsert s (historyNonEmptyCursor h))
          { nonEmptyCursorNext = []
          }
    }

historyUndo :: History s -> Maybe (History s)
historyUndo = historyNonEmptyCursorL nonEmptyCursorSelectPrev

historyRedo :: History s -> Maybe (History s)
historyRedo = historyNonEmptyCursorL nonEmptyCursorSelectNext
