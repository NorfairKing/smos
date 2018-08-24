{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Smos.Actions
    ( module Smos.Actions
    , module Smos.Actions.Utils
    ) where

import Smos.Types

import Smos.Actions.Utils

startHeaderFromEmpty :: Action
startHeaderFromEmpty =
    Action
        { actionName = "startHeaderFromEmpty"
        , actionFunc = modifyEmptyFile startSmosFile
        , actionDescription = "Start a first header in an empty Smos File"
        }

insertEntryAbove :: Action
insertEntryAbove =
    Action
        { actionName = "insertEntryAbove"
        , actionFunc = modifyFileCursor smosFileCursorInsertEntryAbove
        , actionDescription =
              "Insert an entry before the currently selected header, on the same level"
        }

insertEntryBelow :: Action
insertEntryBelow =
    Action
        { actionName = "insertEntryBelow"
        , actionFunc = modifyFileCursor smosFileCursorInsertEntryBelow
        , actionDescription =
              "Insert an entry after the currently selected header, on the same level"
        }

deleteCurrentTree :: Action
deleteCurrentTree =
    Action
        { actionName = "deleteCurrentTree"
        , actionFunc = modifyMFileCursor smosFileCursorDeleteTree
        , actionDescription = "Delete the current entry and all entries below"
        }

moveUpInEntryForest :: Action
moveUpInEntryForest =
    Action
        { actionName = "moveUpInEntryForest"
        , actionFunc = modifyFileCursorM smosFileCursorSelectPrevTree
        , actionDescription =
              "Move the current cursor up to the previous entry in the entry forest"
        }

moveDownInEntryForest :: Action
moveDownInEntryForest =
    Action
        { actionName = "moveDownInEntryForest"
        , actionFunc = modifyFileCursorM smosFileCursorSelectNextTree
        , actionDescription =
              "Move the current cursor down to the previous entry in the entry forest"
        }

moveToFirstEntryForest :: Action
moveToFirstEntryForest =
    Action
        { actionName = "moveToFirstEntryForest"
        , actionFunc = modifyFileCursor smosFileCursorSelectFirstTree
        , actionDescription =
              "Move the current cursor up to the first entry in the entry forest"
        }

moveToLastEntryForest :: Action
moveToLastEntryForest =
    Action
        { actionName = "moveToLastEntryForest"
        , actionFunc = modifyFileCursor smosFileCursorSelectLastTree
        , actionDescription =
              "Move the current cursor down to the last entry in the entry forest"
        }

entrySelectWhole :: Action
entrySelectWhole =
    Action
        { actionName = "entrySelectWhole"
        , actionFunc = modifyEntryCursor entryCursorSelectWhole
        , actionDescription = "Select the whole current Entry"
        }

entrySelectHeader :: Action
entrySelectHeader =
    Action
        { actionName = "entrySelectHeader"
        , actionFunc = modifyEntryCursor entryCursorSelectHeader
        , actionDescription = "Select the current Entry's header"
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
        , actionFunc = modifyEntryCursor entryCursorSelectTags
        , actionDescription = "Select the current Entry's tags"
        }

entrySelectLogbook :: Action
entrySelectLogbook =
    Action
        { actionName = "entrySelectLogbook"
        , actionFunc = modifyEntryCursor entryCursorSelectLogbook
        , actionDescription = "Select the current Entry's logbook"
        }

headerInsert :: ActionUsing Char
headerInsert =
    ActionUsing
        { actionUsingName = "headerInsert"
        , actionUsingFunc =
              \c -> modifyHeaderCursorWhenSelected (headerCursorInsert c)
        , actionUsingDescription =
              "Insert a character into the header in front of the cursor"
        }

headerAppend :: ActionUsing Char
headerAppend =
    ActionUsing
        { actionUsingName = "headerAppend"
        , actionUsingFunc =
              \c -> modifyHeaderCursorWhenSelected (headerCursorAppend c)
        , actionUsingDescription =
              "Append a character into the header in front of the cursor"
        }

headerRemove :: Action
headerRemove =
    Action
        { actionName = "headerRemove"
        , actionFunc = modifyHeaderCursorWhenSelectedM headerCursorRemove
        , actionDescription = "Remove a character from the header"
        }

headerDelete :: Action
headerDelete =
    Action
        { actionName = "headerDelete"
        , actionFunc = modifyHeaderCursorWhenSelectedM headerCursorDelete
        , actionDescription = "Remove a character from the header"
        }

headerMoveLeft :: Action
headerMoveLeft =
    Action
        { actionName = "headerMoveLeft"
        , actionFunc = modifyHeaderCursorWhenSelectedM headerCursorSelectPrev
        , actionDescription = "Move left in the header"
        }

headerMoveRight :: Action
headerMoveRight =
    Action
        { actionName = "headerMoveRight"
        , actionFunc = modifyHeaderCursorWhenSelectedM headerCursorSelectNext
        , actionDescription = "Move right in the header"
        }

headerMoveToStart :: Action
headerMoveToStart =
    Action
        { actionName = "headerMoveToStart"
        , actionFunc = modifyHeaderCursorWhenSelected headerCursorSelectStart
        , actionDescription = "Move to the start of the header"
        }

headerMoveToEnd :: Action
headerMoveToEnd =
    Action
        { actionName = "headerMoveToEnd"
        , actionFunc = modifyHeaderCursorWhenSelected headerCursorSelectEnd
        , actionDescription = "Move to the end of the header"
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
