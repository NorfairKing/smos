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
