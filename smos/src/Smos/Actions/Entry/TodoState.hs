{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Smos.Actions.Entry.TodoState
    ( allTodoStatePlainActions
    , allTodoStateUsingCharActions
    , entrySetTodoState
    , entryToggleTodoState
    , entryUnsetTodoState
    ) where

import Smos.Data

import Smos.Types

import Smos.Actions.Utils

allTodoStatePlainActions :: [Action]
allTodoStatePlainActions =
    entryUnsetTodoState :
    map entrySetTodoState states ++ map entryToggleTodoState states
  where
    states =
        ["TODO", "NEXT", "STARTED", "READY", "WAITING", "DONE", "CANCELLED"]

allTodoStateUsingCharActions :: [ActionUsing Char]
allTodoStateUsingCharActions = []

entrySetTodoState :: TodoState -> Action
entrySetTodoState ts =
    Action
    { actionName = "entrySetTodoState_" <> todoStateText ts
    , actionFunc = modifyMTodoStateM $ const $ Just ts
    , actionDescription =
          "Set the given TODO state of the selected current entry"
    }

entryToggleTodoState :: TodoState -> Action
entryToggleTodoState ts =
    Action
    { actionName = "entryToggleTodoState_" <> todoStateText ts
    , actionFunc =
          modifyMTodoStateM $ \mts ->
              case mts of
                  Nothing -> Just ts
                  Just ts' ->
                      if ts == ts'
                          then Nothing
                          else Just ts
    , actionDescription =
          "Toggle the given TODO state of the selected current entry"
    }

entryUnsetTodoState :: Action
entryUnsetTodoState =
    Action
    { actionName = "entryUnsetTodoState"
    , actionFunc = modifyMTodoStateM $ const Nothing
    , actionDescription = "Unset the TODO state of the selected current entry"
    }
