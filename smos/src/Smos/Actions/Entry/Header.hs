{-# LANGUAGE OverloadedStrings #-}

module Smos.Actions.Entry.Header
  ( allHeaderPlainActions,
    allHeaderUsingCharActions,
    headerInsert,
    headerAppend,
    headerRemove,
    headerDelete,
    headerMoveLeft,
    headerMoveRight,
    headerMoveToStart,
    headerMoveToEnd,
    headerMoveToPrevWord,
    headerMoveToNextWord,
    headerMoveToBeginningOfWord,
    headerMoveToEndOfWord,
  )
where

import Smos.Actions.Utils
import Smos.Types

allHeaderPlainActions :: [Action]
allHeaderPlainActions =
  [ headerRemove,
    headerDelete,
    headerMoveLeft,
    headerMoveRight,
    headerMoveToStart,
    headerMoveToEnd,
    headerMoveToPrevWord,
    headerMoveToNextWord,
    headerMoveToBeginningOfWord,
    headerMoveToEndOfWord
  ]

allHeaderUsingCharActions :: [ActionUsing Char]
allHeaderUsingCharActions = [headerInsert, headerAppend]

headerInsert :: ActionUsing Char
headerInsert =
  ActionUsing
    { actionUsingName = "headerInsert",
      actionUsingFunc = \c -> do
        modifyHeaderCursorWhenSelectedM $ headerCursorInsert c
        unrecordFileCursorHistory,
      actionUsingDescription = "Insert a character into the header in front of the cursor"
    }

headerAppend :: ActionUsing Char
headerAppend =
  ActionUsing
    { actionUsingName = "headerAppend",
      actionUsingFunc = \c -> do
        modifyHeaderCursorWhenSelectedM $ headerCursorAppend c
        unrecordFileCursorHistory,
      actionUsingDescription = "Append a character into the header in front of the cursor"
    }

headerRemove :: Action
headerRemove =
  Action
    { actionName = "headerRemove",
      actionFunc = do
        modifyHeaderCursorWhenSelectedMD headerCursorRemove
        unrecordFileCursorHistory,
      actionDescription = "Remove a character from the header"
    }

headerDelete :: Action
headerDelete =
  Action
    { actionName = "headerDelete",
      actionFunc = do
        modifyHeaderCursorWhenSelectedMD headerCursorDelete
        unrecordFileCursorHistory,
      actionDescription = "Remove a character from the header"
    }

headerMoveLeft :: Action
headerMoveLeft =
  Action
    { actionName = "headerMoveLeft",
      actionFunc = modifyHeaderCursorWhenSelectedM headerCursorSelectPrev,
      actionDescription = "Move left in the header"
    }

headerMoveRight :: Action
headerMoveRight =
  Action
    { actionName = "headerMoveRight",
      actionFunc = modifyHeaderCursorWhenSelectedM headerCursorSelectNext,
      actionDescription = "Move right in the header"
    }

headerMoveToStart :: Action
headerMoveToStart =
  Action
    { actionName = "headerMoveToStart",
      actionFunc = modifyHeaderCursorWhenSelected headerCursorSelectStart,
      actionDescription = "Move to the start of the header"
    }

headerMoveToEnd :: Action
headerMoveToEnd =
  Action
    { actionName = "headerMoveToEnd",
      actionFunc = modifyHeaderCursorWhenSelected headerCursorSelectEnd,
      actionDescription = "Move to the end of the header"
    }

headerMoveToPrevWord :: Action
headerMoveToPrevWord =
  Action
    { actionName = "headerMoveToPrevWord",
      actionFunc = modifyHeaderCursorWhenSelected headerCursorSelectPrevWord,
      actionDescription = "Move to the previous word in the header"
    }

headerMoveToNextWord :: Action
headerMoveToNextWord =
  Action
    { actionName = "headerMoveToNextWord",
      actionFunc = modifyHeaderCursorWhenSelected headerCursorSelectNextWord,
      actionDescription = "Move to the next word in the header"
    }

headerMoveToBeginningOfWord :: Action
headerMoveToBeginningOfWord =
  Action
    { actionName = "headerMoveToBeginningOfWord",
      actionFunc = modifyHeaderCursorWhenSelected headerCursorSelectBeginWord,
      actionDescription = "Move to the beginning of a word in the header"
    }

headerMoveToEndOfWord :: Action
headerMoveToEndOfWord =
  Action
    { actionName = "headerMoveToEndOfWord",
      actionFunc = modifyHeaderCursorWhenSelected headerCursorSelectEndWord,
      actionDescription = "Move to the end of a word in the header"
    }
