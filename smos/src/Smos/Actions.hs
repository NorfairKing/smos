{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Smos.Actions
    ( module Smos.Actions
    , module Smos.Actions.Entry
    , module Smos.Actions.Forest
    , module Smos.Actions.Header
    , module Smos.Actions.Utils
    ) where

import Smos.Types

import Smos.Actions.Entry
import Smos.Actions.Forest
import Smos.Actions.Header
import Smos.Actions.Utils

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
