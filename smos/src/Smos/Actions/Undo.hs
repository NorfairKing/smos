{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Smos.Actions.Undo
    ( allUndoPlainActions
    , allUndoUsingCharActions
    , undo
    ) where

import Smos.Types

allUndoPlainActions :: [Action]
allUndoPlainActions = [undo]

allUndoUsingCharActions :: [ActionUsing Char]
allUndoUsingCharActions = []

undo :: Action
undo =
    Action
    { actionName = "undo"
    , actionDescription = "Undo the last action"
    , actionFunc =
          modify $ \ss ->
              case smosStateCursorHistory ss of
                  [] -> ss
                  [_] -> ss
                  (_:c:cs) ->
                      ss {smosStateCursor = c, smosStateCursorHistory = cs}
    }
