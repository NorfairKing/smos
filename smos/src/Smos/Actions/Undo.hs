{-# LANGUAGE OverloadedStrings #-}

module Smos.Actions.Undo
  ( allUndoPlainActions
  , allUndoUsingCharActions
  , undo
  , undoAny
  ) where

import Smos.Types

allUndoPlainActions :: [Action]
allUndoPlainActions = [undo, undoAny]

allUndoUsingCharActions :: [ActionUsing Char]
allUndoUsingCharActions = []

-- Undo a non-movement action
undo :: Action
undo =
  Action
    { actionName = "undo"
    , actionDescription = "Undo the last non-movement action"
    , actionFunc =
        modify $ \ss ->
          let go [] = ss
              go (c:cs) =
                if rebuildEditorCursor c ==
                   rebuildEditorCursor (smosStateCursor ss)
                  then go cs
                  else ss {smosStateCursor = c, smosStateCursorHistory = cs}
           in go $ drop 1 $ smosStateCursorHistory ss
    }

-- Undo any action, even movements
undoAny :: Action
undoAny =
  Action
    { actionName = "undoAny"
    , actionDescription = "Undo the last action, even if it was a movement"
    , actionFunc =
        modify $ \ss ->
          case drop 1 $ smosStateCursorHistory ss of
            [] -> ss
            (c:cs) -> ss {smosStateCursor = c, smosStateCursorHistory = cs}
    }
