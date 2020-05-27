{-# LANGUAGE OverloadedStrings #-}

module Smos.Actions.Undo
  ( allUndoPlainActions,
    allUndoUsingCharActions,
    undo,
    undoAny,
  )
where

import Smos.Types

allUndoPlainActions :: [Action]
allUndoPlainActions = [undo, undoAny]

allUndoUsingCharActions :: [ActionUsing Char]
allUndoUsingCharActions = []

-- Undo a non-movement action
undo :: Action
undo =
  Action
    { actionName = "undo",
      actionDescription = "Undo the last non-movement action",
      actionFunc = undefined
    }

-- Undo any action, even movements
undoAny :: Action
undoAny =
  Action
    { actionName = "undoAny",
      actionDescription = "Undo the last action, even if it was a movement",
      actionFunc = undefined
    }
