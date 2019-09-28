{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Smos.Actions.Entry.TodoState
  ( allTodoStatePlainActions
  , allTodoStateUsingCharActions
  , entrySetTodoState
  , entryToggleTodoState
  , entryUnsetTodoState
  , subtreeSetTodoState
  , subtreeUnsetTodoState
  ) where

import Data.Time

import Smos.Data

import Smos.Types

import Smos.Actions.Utils

allTodoStatePlainActions :: [Action]
allTodoStatePlainActions =
  entryUnsetTodoState :
  subtreeUnsetTodoState : do
    a <- [entrySetTodoState, entryToggleTodoState, subtreeSetTodoState]
    s <- states
    pure $ a s
  where
    states = ["TODO", "NEXT", "STARTED", "READY", "WAITING", "DONE", "CANCELLED"]

allTodoStateUsingCharActions :: [ActionUsing Char]
allTodoStateUsingCharActions = []

entrySetTodoState :: TodoState -> Action
entrySetTodoState ts =
  Action
    { actionName = "entrySetTodoState_" <> ActionName (todoStateText ts)
    , actionFunc = modifyMTodoStateM $ const $ Just ts
    , actionDescription = "Set the given TODO state of the selected current entry"
    }

entryToggleTodoState :: TodoState -> Action
entryToggleTodoState ts =
  Action
    { actionName = "entryToggleTodoState_" <> ActionName (todoStateText ts)
    , actionFunc =
        modifyMTodoStateM $ \case
          Nothing -> Just ts
          Just ts' ->
            if ts == ts'
              then Nothing
              else Just ts
    , actionDescription = "Toggle the given TODO state of the selected current entry"
    }

entryUnsetTodoState :: Action
entryUnsetTodoState =
  Action
    { actionName = "entryUnsetTodoState"
    , actionFunc = modifyMTodoStateM $ const Nothing
    , actionDescription = "Unset the TODO state of the selected current entry"
    }

subtreeSetTodoState :: TodoState -> Action
subtreeSetTodoState ts =
  Action
    { actionName = "subtreeSetTodoState_" <> ActionName (todoStateText ts)
    , actionFunc =
        modifyFileCursorS $ \sfc -> do
          now <- liftIO getCurrentTime
          pure $ smosFileSubtreeSetTodoState now (Just ts) sfc
    , actionDescription = "Set the given TODO state on all of the entries in the current subtree"
    }

subtreeUnsetTodoState :: Action
subtreeUnsetTodoState =
  Action
    { actionName = "subtreeUnsetTodoState"
    , actionFunc =
        modifyFileCursorS $ \sfc -> do
          now <- liftIO getCurrentTime
          pure $ smosFileSubtreeSetTodoState now Nothing sfc
    , actionDescription = "Unset the TODO state on all of the entries in the current subtree"
    }
