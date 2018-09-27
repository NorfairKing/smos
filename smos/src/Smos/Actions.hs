{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Smos.Actions
    ( Action(..)
    , ActionUsing(..)
    , AnyAction(..)
    , module Smos.Actions
    , module Smos.Actions.Entry
    , module Smos.Actions.Forest
    , module Smos.Actions.Header
    , module Smos.Actions.Logbook
    , module Smos.Actions.Tags
    , module Smos.Actions.Undo
    , module Smos.Actions.Utils
    ) where

import Smos.Types

import Smos.Actions.Entry
import Smos.Actions.Forest
import Smos.Actions.Header
import Smos.Actions.Logbook
import Smos.Actions.Tags
import Smos.Actions.Undo
import Smos.Actions.Utils

allActions :: [AnyAction]
allActions =
    map PlainAction allPlainActions ++ map UsingCharAction allUsingCharActions

allPlainActions :: [Action]
allPlainActions =
    concat
        [ [ startHeaderFromEmptyAndSelectHeader
          , selectHelp
          , selectEditor
          , showDebug
          , hideDebug
          , toggleDebug
          ]
        , allHeaderPlainActions
        , allLogbookPlainActions
        , allEntryPlainActions
        , allTagsPlainActions
        , allForestPlainActions
        , allUndoPlainActions
        ]

allUsingCharActions :: [ActionUsing Char]
allUsingCharActions =
    concat
        [ allHeaderUsingCharActions
        , allEntryUsingCharActions
        , allForestUsingCharActions
        , allUndoUsingCharActions
        ]

startHeaderFromEmptyAndSelectHeader :: Action
startHeaderFromEmptyAndSelectHeader =
    Action
    { actionName = "startHeaderFromEmptyAndSelectHeader"
    , actionFunc = modifyEmptyFile startSmosFile
    , actionDescription = "Start a first header in an empty Smos File"
    }

selectHelp :: Action
selectHelp =
    Action
    { actionName = "selectHelp"
    , actionFunc = modifyEditorCursor editorCursorSelectHelp
    , actionDescription = "Show the (contextual) help screen"
    }

selectEditor :: Action
selectEditor =
    Action
    { actionName = "selectEditor"
    , actionFunc = modifyEditorCursor editorCursorSelectEditor
    , actionDescription = "Hide the help screen"
    }

showDebug :: Action
showDebug =
    Action
    { actionName = "showDebug"
    , actionFunc = modifyEditorCursor editorCursorShowDebug
    , actionDescription = "Show the debug screen"
    }

hideDebug :: Action
hideDebug =
    Action
    { actionName = "hideDebug"
    , actionFunc = modifyEditorCursor editorCursorHideDebug
    , actionDescription = "Hide the debug screen"
    }

toggleDebug :: Action
toggleDebug =
    Action
    { actionName = "toggleDebug"
    , actionFunc = modifyEditorCursor editorCursorToggleDebug
    , actionDescription = "Toggle the debug page to be shown"
    }
