{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Smos.Actions
    ( Action(..)
    , ActionUsing(..)
    , AnyAction(..)
    , module Smos.Actions
    , module Smos.Actions.Contents
    , module Smos.Actions.Entry
    , module Smos.Actions.Forest
    , module Smos.Actions.Header
    , module Smos.Actions.Help
    , module Smos.Actions.Logbook
    , module Smos.Actions.Tags
    , module Smos.Actions.Timestamps
    , module Smos.Actions.Undo
    , module Smos.Actions.Utils
    ) where

import Smos.Data

import Smos.Types

import Smos.Actions.Help
import Smos.Actions.Contents
import Smos.Actions.Entry
import Smos.Actions.Forest
import Smos.Actions.Header
import Smos.Actions.Logbook
import Smos.Actions.Tags
import Smos.Actions.Timestamps
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
        , allContentsPlainActions
        , allEntryPlainActions
        , allForestPlainActions
        , allHeaderPlainActions
        , allLogbookPlainActions
        , allTagsPlainActions
        , allTimestampsPlainActions
        , allUndoPlainActions
        ]

allUsingCharActions :: [ActionUsing Char]
allUsingCharActions =
    concat
        [ allContentsUsingCharActions
        , allEntryUsingCharActions
        , allForestUsingCharActions
        , allHeaderUsingCharActions
        , allTimestampsUsingCharActions
        , allUndoUsingCharActions
        ]

saveFile :: Action
saveFile =
    Action
        { actionName = "saveFile"
        , actionFunc =
              do SmosState {..} <- get
                 let sf' = rebuildEditorCursor smosStateCursor
                 when (smosStateStartSmosFile /= Just sf') $
                     liftIO $ writeSmosFile smosStateFilePath sf'
        , actionDescription = "Save the current file"
        }

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
        , actionFunc = modifyEditorCursorS $ \ec -> do
            km <- asks configKeyMap
            pure $ editorCursorSwitchToHelp km ec
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
