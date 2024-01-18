{-# LANGUAGE DeriveGeneric #-}

module Smos.Undo where

import Control.DeepSeq
import Cursor.List
import Data.Validity
import GHC.Generics (Generic)

-- | An undo stack with actions of type 'a'.
--
-- In order for this to work, you need a type 'b' as your history.
-- There must be a function 'applyUndo' that applies an undo of an 'a' to your current history b,
-- and a function 'applyRedo' that applies a redo of an 'a' to your current history b.
-- These two functions must work such that applying an undo after the corresponding redo
-- results in the same 'b' as not doing anything.
-- The same must hold in the opposite direction when applying the undo first and then the redo.
--
-- See 'Smos.UndoSpec' for an example.
newtype UndoStack a = UndoStack {undoStackListCursor :: ListCursor a}
  deriving (Show, Eq, Generic)

instance (Validity a) => Validity (UndoStack a)

instance (NFData a) => NFData (UndoStack a)

emptyUndoStack :: UndoStack a
emptyUndoStack = UndoStack {undoStackListCursor = emptyListCursor}

undoStackPush :: a -> UndoStack a -> UndoStack a
undoStackPush u us =
  UndoStack
    { undoStackListCursor = fst $ listCursorSplit $ listCursorInsert u (undoStackListCursor us)
    }

undoStackUndo :: UndoStack a -> Maybe (a, UndoStack a)
undoStackUndo (UndoStack lc) = do
  (,) <$> listCursorPrevItem lc <*> (UndoStack <$> listCursorSelectPrev lc)

undoStackRedo :: UndoStack a -> Maybe (a, UndoStack a)
undoStackRedo (UndoStack lc) = do
  (,) <$> listCursorNextItem lc <*> (UndoStack <$> listCursorSelectNext lc)

undoStackUndoLength :: UndoStack a -> Word
undoStackUndoLength = fromIntegral . length . listCursorPrev . undoStackListCursor

undoStackRedoLength :: UndoStack a -> Word
undoStackRedoLength = fromIntegral . length . listCursorNext . undoStackListCursor
