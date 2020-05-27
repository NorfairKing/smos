{-# LANGUAGE DeriveGeneric #-}

module Smos.Undo where

import Cursor.List
import Data.Validity
import GHC.Generics (Generic)

-- | An undo stack with undo actions of type 'a' and redo actions of type 'b'.
--
-- In order for this to work, you need a third type 'c' as your history.
-- There must be a function 'applyUndo' that applies an undo 'a' to your current history c,
-- and a function 'applyRedo' that applies a redo 'b' to your current history c.
-- These two functions must work such that applying an undo after the corresponding redo
-- results in the same 'c' as not doing anything.
-- The same must hold in the opposite direction when applying the undo first and then the redo.
newtype UndoStack a b
  = UndoStack {undoStackListCursor :: ListCursor (Undo a b)}
  deriving (Show, Eq, Generic)

instance (Validity a, Validity b) => Validity (UndoStack a b)

data Undo a b = Undo {undoAction :: a, redoAction :: b}
  deriving (Show, Eq, Generic)

instance (Validity a, Validity b) => Validity (Undo a b)

emptyUndoStack :: UndoStack a b
emptyUndoStack = UndoStack {undoStackListCursor = emptyListCursor}

undoStackPush :: Undo a b -> UndoStack a b -> UndoStack a b
undoStackPush = undefined

undoStackUndo :: UndoStack a b -> Maybe (a, UndoStack a b)
undoStackUndo = undefined

undoStackRedo :: UndoStack a b -> Maybe (b, UndoStack a b)
undoStackRedo = undefined
