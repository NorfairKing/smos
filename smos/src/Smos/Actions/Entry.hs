{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Smos.Actions.Entry
    ( allEntryPlainActions
    , allEntryUsingCharActions
    , entrySelectWhole
    , entrySelectHeaderAtStart
    , entrySelectHeaderAtEnd
    , entrySelectContents
    , entrySelectProperties
    , entrySelectTimestamps
    , entrySelectStateHistory
    , entrySelectTags
    , entrySelectLogbook
    , module Smos.Actions.Entry.TodoState
    ) where

import Data.Maybe

import Smos.Types

import Smos.Actions.Utils

import Smos.Actions.Entry.TodoState

allEntryPlainActions :: [Action]
allEntryPlainActions =
    [ entrySelectWhole
    , entrySelectHeaderAtStart
    , entrySelectHeaderAtEnd
    , entrySelectContents
    , entrySelectProperties
    , entrySelectTimestamps
    , entrySelectStateHistory
    , entrySelectTags
    , entrySelectLogbook
    ] ++
    allTodoStatePlainActions

allEntryUsingCharActions :: [ActionUsing Char]
allEntryUsingCharActions = [] ++ allTodoStateUsingCharActions

entrySelectWhole :: Action
entrySelectWhole =
    Action
    { actionName = "entrySelectWhole"
    , actionFunc = modifyEntryCursor entryCursorSelectWhole
    , actionDescription = "Select the whole current Entry"
    }

entrySelectHeaderAtStart :: Action
entrySelectHeaderAtStart =
    Action
    { actionName = "entrySelectHeaderAtStart"
    , actionFunc = modifyEntryCursor entryCursorSelectHeaderAtStart
    , actionDescription =
          "Select the current Entry's header and select the start"
    }

entrySelectHeaderAtEnd :: Action
entrySelectHeaderAtEnd =
    Action
    { actionName = "entrySelectHeaderAtEnd"
    , actionFunc = modifyEntryCursor entryCursorSelectHeaderAtEnd
    , actionDescription = "Select the current Entry's header and select the end"
    }

entrySelectContents :: Action
entrySelectContents =
    Action
    { actionName = "entrySelectContents"
    , actionFunc = modifyEntryCursor entryCursorSelectContents
    , actionDescription = "Select the current Entry's contents"
    }

entrySelectTimestamps :: Action
entrySelectTimestamps =
    Action
    { actionName = "entrySelectTimestamps"
    , actionFunc = modifyEntryCursor entryCursorSelectTimestamps
    , actionDescription = "Select the current Entry's timestamps"
    }

entrySelectProperties :: Action
entrySelectProperties =
    Action
    { actionName = "entrySelectProperties"
    , actionFunc = modifyEntryCursor entryCursorSelectProperties
    , actionDescription = "Select the current Entry's properties"
    }

entrySelectStateHistory :: Action
entrySelectStateHistory =
    Action
    { actionName = "entrySelectStateHistory"
    , actionFunc = modifyEntryCursor entryCursorSelectStateHistory
    , actionDescription = "Select the current Entry's state history"
    }

entrySelectTags :: Action
entrySelectTags =
    Action
    { actionName = "entrySelectTags"
    , actionFunc =
          do modifyEntryCursor entryCursorSelectTags
             modifyMTagsCursor $ fromMaybe (singletonTagsCursor "")
    , actionDescription = "Select the current Entry's tags"
    }

entrySelectLogbook :: Action
entrySelectLogbook =
    Action
    { actionName = "entrySelectLogbook"
    , actionFunc = modifyEntryCursor entryCursorSelectLogbook
    , actionDescription = "Select the current Entry's logbook"
    }
