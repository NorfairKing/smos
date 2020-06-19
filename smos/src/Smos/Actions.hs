{-# LANGUAGE OverloadedStrings #-}

module Smos.Actions
  ( Action (..),
    ActionUsing (..),
    AnyAction (..),
    module Smos.Actions,
    module Smos.Actions.Browser,
    module Smos.Actions.Convenience,
    module Smos.Actions.Entry,
    module Smos.Actions.Entry.Contents,
    module Smos.Actions.Entry.Header,
    module Smos.Actions.Entry.Logbook,
    module Smos.Actions.Entry.Properties,
    module Smos.Actions.Entry.Tags,
    module Smos.Actions.Entry.Timestamps,
    module Smos.Actions.File,
    module Smos.Actions.Forest,
    module Smos.Actions.Help,
    module Smos.Actions.Report,
    module Smos.Actions.Undo,
    module Smos.Actions.Utils,
  )
where

import Smos.Actions.Browser
import Smos.Actions.Convenience
import Smos.Actions.Entry
import Smos.Actions.Entry.Contents
import Smos.Actions.Entry.Header
import Smos.Actions.Entry.Logbook
import Smos.Actions.Entry.Properties
import Smos.Actions.Entry.Tags
import Smos.Actions.Entry.Timestamps
import Smos.Actions.File
import Smos.Actions.Forest
import Smos.Actions.Help
import Smos.Actions.Report
import Smos.Actions.Undo
import Smos.Actions.Utils
import Smos.Types

allActions :: [AnyAction]
allActions = map PlainAction allPlainActions ++ map UsingCharAction allUsingCharActions

allPlainActions :: [Action]
allPlainActions =
  concat
    [ [ startHeaderFromEmptyAndSelectHeader,
        selectHelp,
        selectEditor,
        showDebug,
        hideDebug,
        toggleDebug,
        saveFile,
        stop
      ],
      allContentsPlainActions,
      allEntryPlainActions,
      allForestPlainActions,
      allHeaderPlainActions,
      allLogbookPlainActions,
      allTagsPlainActions,
      allPropertiesPlainActions,
      allTimestampsPlainActions,
      allUndoPlainActions,
      allConveniencePlainActions,
      allPlainBrowserActions,
      allHelpPlainActions,
      allPlainReportNextActions
    ]

allUsingCharActions :: [ActionUsing Char]
allUsingCharActions =
  concat
    [ allContentsUsingCharActions,
      allEntryUsingCharActions,
      allForestUsingCharActions,
      allHeaderUsingCharActions,
      allTimestampsUsingCharActions,
      allTagsUsingCharActions,
      allPropertiesUsingCharActions,
      allUndoUsingCharActions,
      allBrowserUsingCharActions,
      allHelpUsingCharActions,
      allReportNextActionsUsingActions
    ]

startHeaderFromEmptyAndSelectHeader :: Action
startHeaderFromEmptyAndSelectHeader =
  Action
    { actionName = "startHeaderFromEmptyAndSelectHeader",
      actionFunc = modifyEmptyFile startSmosFile,
      actionDescription = "Start a first header in an empty Smos File"
    }

selectHelp :: Action
selectHelp =
  Action
    { actionName = "selectHelp",
      actionFunc = modifyEditorCursorS $ \ec -> do
        km <- asks configKeyMap
        pure $ editorCursorSwitchToHelp km ec,
      actionDescription = "Show the (contextual) help screen"
    }

selectEditor :: Action
selectEditor =
  Action
    { actionName = "selectEditor",
      actionFunc = modifyEditorCursor editorCursorSwitchToFile,
      actionDescription = "Hide the help screen"
    }

showDebug :: Action
showDebug =
  Action
    { actionName = "showDebug",
      actionFunc = modifyEditorCursor editorCursorShowDebug,
      actionDescription = "Show the debug screen"
    }

hideDebug :: Action
hideDebug =
  Action
    { actionName = "hideDebug",
      actionFunc = modifyEditorCursor editorCursorHideDebug,
      actionDescription = "Hide the debug screen"
    }

toggleDebug :: Action
toggleDebug =
  Action
    { actionName = "toggleDebug",
      actionFunc = modifyEditorCursor editorCursorToggleDebug,
      actionDescription = "Toggle the debug page to be shown"
    }
