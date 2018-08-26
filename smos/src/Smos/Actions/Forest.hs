{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Smos.Actions.Forest
    ( forestInsertEntryBefore
    , forestInsertEntryBeforeAndSelectHeader
    , forestInsertEntryAfter
    , forestInsertEntryAfterAndSelectHeader
    , forestDeleteCurrentTree
    , forestMoveUp
    , forestMoveDown
    , forestMoveToFirstTree
    , forestMoveToLastTree
    ) where

import Smos.Types

import Smos.Actions.Utils

forestInsertEntryBefore :: Action
forestInsertEntryBefore =
    Action
        { actionName = "forestInsertEntryBefore"
        , actionFunc = modifyFileCursor smosFileCursorInsertEntryAfter
        , actionDescription =
              "Insert an entry before the currently selected header, on the same level"
        }

forestInsertEntryBeforeAndSelectHeader :: Action
forestInsertEntryBeforeAndSelectHeader =
    Action
        { actionName = "forestInsertEntryBeforeAndSelectHeader"
        , actionFunc =
              modifyFileCursor smosFileCursorInsertEntryBeforeAndSelectHeader
        , actionDescription =
              "Insert an entry before the currently selected entry, on the same level, and select its header"
        }

forestInsertEntryAfter :: Action
forestInsertEntryAfter =
    Action
        { actionName = "forestInsertEntryAfter"
        , actionFunc = modifyFileCursor smosFileCursorInsertEntryBefore
        , actionDescription =
              "Insert an entry after the currently selected entry, on the same level"
        }

forestInsertEntryAfterAndSelectHeader :: Action
forestInsertEntryAfterAndSelectHeader =
    Action
        { actionName = "forestInsertEntryAfterAndSelectHeader"
        , actionFunc =
              modifyFileCursor smosFileCursorInsertEntryAfterAndSelectHeader
        , actionDescription =
              "Insert an entry after the currently selected entry, on the same level, and select its header"
        }

forestDeleteCurrentTree :: Action
forestDeleteCurrentTree =
    Action
        { actionName = "forestDeleteCurrentTree"
        , actionFunc = modifyMFileCursor smosFileCursorDeleteTree
        , actionDescription = "Delete the current entry and all entries below"
        }

forestMoveUp :: Action
forestMoveUp =
    Action
        { actionName = "forestMoveUp"
        , actionFunc = modifyFileCursorM smosFileCursorSelectPrevTree
        , actionDescription =
              "Move the current cursor up to the previous entry in the entry forest"
        }

forestMoveDown :: Action
forestMoveDown =
    Action
        { actionName = "forestMoveDown"
        , actionFunc = modifyFileCursorM smosFileCursorSelectNextTree
        , actionDescription =
              "Move the current cursor down to the previous entry in the entry forest"
        }

forestMoveToFirstTree :: Action
forestMoveToFirstTree =
    Action
        { actionName = "forestMoveToFirstTree"
        , actionFunc = modifyFileCursor smosFileCursorSelectFirstTree
        , actionDescription =
              "Move the current cursor up to the first entry in the entry forest"
        }

forestMoveToLastTree :: Action
forestMoveToLastTree =
    Action
        { actionName = "forestMoveToLastTree"
        , actionFunc = modifyFileCursor smosFileCursorSelectLastTree
        , actionDescription =
              "Move the current cursor down to the last entry in the entry forest"
        }
