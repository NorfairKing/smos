{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Smos.Actions.Forest
    ( forestInsertEntryAbove
    , forestInsertEntryBelow
    , forestDeleteCurrentTree
    , forestMoveUp
    , forestMoveDown
    , forestMoveToFirstTree
    , forestMoveToLastTree
    ) where

import Smos.Types

import Smos.Actions.Utils

forestInsertEntryAbove :: Action
forestInsertEntryAbove =
    Action
        { actionName = "forestInsertEntryAbove"
        , actionFunc = modifyFileCursor smosFileCursorInsertEntryAbove
        , actionDescription =
              "Insert an entry before the currently selected header, on the same level"
        }

forestInsertEntryBelow :: Action
forestInsertEntryBelow =
    Action
        { actionName = "forestInsertEntryBelow"
        , actionFunc = modifyFileCursor smosFileCursorInsertEntryBelow
        , actionDescription =
              "Insert an entry after the currently selected header, on the same level"
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
