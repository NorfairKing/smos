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
    action "Start a first header in an empty Smos File" $
    modifyEmptyFile startSmosFile

insertEntryAbove :: Action
insertEntryAbove =
    action
        "Insert an entry before the currently selected header, on the same level" $
    modifyFileCursor smosFileCursorInsertEntryAbove

insertEntryBelow :: Action
insertEntryBelow =
    action
        "Insert an entry after the currently selected header, on the same level" $
    modifyFileCursor smosFileCursorInsertEntryBelow

deleteCurrentTree :: Action
deleteCurrentTree =
    action "Delete the current entry and all entries below" $
    modifyMFileCursor smosFileCursorDeleteTree

moveUpInEntryForest :: Action
moveUpInEntryForest =
    action
        "Move the current cursor up to the previous entry in the entry forest" $
    modifyFileCursorM smosFileCursorSelectPrevTree

moveDownInEntryForest :: Action
moveDownInEntryForest =
    action
        "Move the current cursor down to the previous entry in the entry forest" $
    modifyFileCursorM smosFileCursorSelectNextTree

moveToFirstEntryForest :: Action
moveToFirstEntryForest =
    action "Move the current cursor up to the first entry in the entry forest" $
    modifyFileCursor smosFileCursorSelectFirstTree

moveToLastEntryForest :: Action
moveToLastEntryForest =
    action "Move the current cursor down to the last entry in the entry forest" $
    modifyFileCursor smosFileCursorSelectLastTree

toggleHelp :: Action
toggleHelp =
    action "Toggle the help page to be shown" $
    modify (\ss -> ss {smosStateShowHelp = not $ smosStateShowHelp ss})

toggleDebug :: Action
toggleDebug =
    action "Toggle the debug info" $
    modify (\ss -> ss {smosStateShowDebug = not $ smosStateShowDebug ss})

entrySelectWhole :: Action
entrySelectWhole =
    action "Select the whole current Entry" $
    modifyEntryCursor entryCursorSelectWhole

entrySelectHeader :: Action
entrySelectHeader =
    action "Select the current Entry's header" $
    modifyEntryCursor entryCursorSelectHeader

entrySelectContents :: Action
entrySelectContents =
    action "Select the current Entry's contents" $
    modifyEntryCursor entryCursorSelectContents

entrySelectTimestamps :: Action
entrySelectTimestamps =
    action "Select the current Entry's timestamps" $
    modifyEntryCursor entryCursorSelectTimestamps

entrySelectProperties :: Action
entrySelectProperties =
    action "Select the current Entry's properties" $
    modifyEntryCursor entryCursorSelectProperties

entrySelectStateHistory :: Action
entrySelectStateHistory =
    action "Select the current Entry's state history" $
    modifyEntryCursor entryCursorSelectStateHistory

entrySelectTags :: Action
entrySelectTags =
    action "Select the current Entry's tags" $
    modifyEntryCursor entryCursorSelectTags

entrySelectLogbook :: Action
entrySelectLogbook =
    action "Select the current Entry's logbook" $
    modifyEntryCursor entryCursorSelectLogbook

headerInsert :: ActionUsing Char
headerInsert =
    actionUsing "Insert a character into the header in front of the cursor" $ \c ->
        modifyHeaderCursorWhenSelected (headerCursorInsert c)

headerAppend :: ActionUsing Char
headerAppend =
    actionUsing "Append a character onto the header behind the cursor" $ \c ->
        modifyHeaderCursorWhenSelected (headerCursorAppend c)

headerRemove :: Action
headerRemove =
    action "Remove a character from the header" $
    modifyHeaderCursorWhenSelectedM headerCursorRemove

headerDelete :: Action
headerDelete =
    action "Remove a character from the header" $
    modifyHeaderCursorWhenSelectedM headerCursorDelete

headerMoveLeft :: Action
headerMoveLeft =
    action "Move left in the header" $
    modifyHeaderCursorWhenSelectedM headerCursorSelectPrev

headerMoveRight :: Action
headerMoveRight =
    action "Move right in the header" $
    modifyHeaderCursorWhenSelectedM headerCursorSelectNext

headerMoveToStart :: Action
headerMoveToStart =
    action "Move to the start of the header" $
    modifyHeaderCursorWhenSelected headerCursorSelectStart

headerMoveToEnd :: Action
headerMoveToEnd =
    action "Move to the end of the header" $
    modifyHeaderCursorWhenSelected headerCursorSelectEnd
