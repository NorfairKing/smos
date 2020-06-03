{-# LANGUAGE DeriveGeneric #-}

module Smos.History
  ( History (..),
    historyNonEmptyCursorL,
    historyPresentL,
    startingHistory,
    historyPresent,
    historyPush,
    historyMod,
    historyModM,
    historyUndo,
    historyRedo,
    historyUndoLength,
    historyRedoLength,
    historyForgetLatest,
  )
where

import Control.DeepSeq
import Control.Monad
import Cursor.Simple.List.NonEmpty
import Cursor.Types
import Data.Validity
import GHC.Generics (Generic)
import Lens.Micro

-- | An simple state-based history for a state 's'
--
-- TODO: make this bounded?
newtype History s
  = History {historyNonEmptyCursor :: NonEmptyCursor s}
  deriving (Show, Eq, Generic)

instance Validity s => Validity (History s)

instance NFData s => NFData (History s)

-- |
--
-- NOTE: this lens does _NOT_ record history.
historyNonEmptyCursorL :: Lens' (History s) (NonEmptyCursor s)
historyNonEmptyCursorL = lens historyNonEmptyCursor $ \h lc -> h {historyNonEmptyCursor = lc}

-- |
--
-- NOTE: this lens does _NOT_ record history.
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
        (nonEmptyCursorAppendAndSelect s (historyNonEmptyCursor h))
          { nonEmptyCursorNext = []
          }
    }

historyMod :: (s -> s) -> History s -> History s
historyMod func h =
  historyPush (func (historyPresent h)) h

historyModM :: Functor f => (s -> f s) -> History s -> f (History s)
historyModM func h =
  (`historyPush` h) <$> func (historyPresent h)

historyUndo :: History s -> Maybe (History s)
historyUndo = historyNonEmptyCursorL nonEmptyCursorSelectPrev

historyRedo :: History s -> Maybe (History s)
historyRedo = historyNonEmptyCursorL nonEmptyCursorSelectNext

historyUndoLength :: History s -> Word
historyUndoLength = fromIntegral . length . nonEmptyCursorPrev . historyNonEmptyCursor

historyRedoLength :: History s -> Word
historyRedoLength = fromIntegral . length . nonEmptyCursorNext . historyNonEmptyCursor

-- Nothing if there was nothing to forgot.
historyForgetLatest :: History s -> Maybe (History s)
historyForgetLatest = historyNonEmptyCursorL $ nonEmptyCursorSelectPrev >=> dullMDelete . nonEmptyCursorDeleteElemAndSelectNext
