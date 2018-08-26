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

startHeaderFromEmpty :: Action
startHeaderFromEmpty =
    Action
        { actionName = "startHeaderFromEmpty"
        , actionFunc = modifyEmptyFile startSmosFile
        , actionDescription = "Start a first header in an empty Smos File"
        }

showHelp :: Action
showHelp =
    Action
        { actionName = "showHelp"
        , actionFunc = modifyEditorCursor editorCursorShowHelp
        , actionDescription = "Show the (contextual) help screen"
        }

hideHelp :: Action
hideHelp =
    Action
        { actionName = "hideHelp"
        , actionFunc = modifyEditorCursor editorCursorHideHelp
        , actionDescription = "Hide the help screen"
        }

toggleHelp :: Action
toggleHelp =
    Action
        { actionName = "toggleHelp"
        , actionFunc = modifyEditorCursor editorCursorToggleHelp
        , actionDescription = "Toggle the help page to be shown"
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
