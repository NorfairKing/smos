{-# LANGUAGE OverloadedStrings #-}

module Smos.Actions.Undo
  ( allUndoPlainActions,
    allUndoUsingCharActions,
    undo,
    undoAny,
    redo,
    redoAny,
  )
where

import Data.Function
import Smos.Actions.Utils
import Smos.History
import Smos.Types

allUndoPlainActions :: [Action]
allUndoPlainActions = [undo, undoAny, redo, redoAny]

allUndoUsingCharActions :: [ActionUsing Char]
allUndoUsingCharActions = []

-- Undo a non-movement action
undo :: Action
undo =
  Action
    { actionName = "undo",
      actionDescription = "Undo the last non-movement action",
      actionFunc = modifyMFileCursorMHistoryM go
    }
  where
    go h = do
      h' <- historyUndo h
      if ((==) `on` (fmap rebuildSmosFileCursor . historyPresent)) h' h
        then go h'
        else pure h'

-- Undo any action, even movements
undoAny :: Action
undoAny =
  Action
    { actionName = "undoAny",
      actionDescription = "Undo the last action, even if it was a movement",
      actionFunc = modifyMFileCursorMHistoryM historyUndo
    }

-- Undo a non-movement action
redo :: Action
redo =
  Action
    { actionName = "redo",
      actionDescription = "Redo the last non-movement action",
      actionFunc = modifyMFileCursorMHistoryM go
    }
  where
    go h = do
      h' <- historyRedo h
      if ((==) `on` (fmap rebuildSmosFileCursor . historyPresent)) h' h
        then go h'
        else pure h'

-- Undo any action, even movements
redoAny :: Action
redoAny =
  Action
    { actionName = "redoAny",
      actionDescription = "Redo the last action, even if it was a movement",
      actionFunc = modifyMFileCursorMHistoryM historyRedo
    }
