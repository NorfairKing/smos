{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Smos.Actions.Contents
  ( allContentsPlainActions
  , allContentsUsingCharActions
  , contentsInsert
  , contentsInsertNewline
  , contentsAppend
  , contentsAppendNewline
  , contentsRemove
  , contentsDelete
  , contentsMoveLeft
  , contentsMoveRight
  , contentsMoveUp
  , contentsMoveDown
  , contentsMoveToStartOfLine
  , contentsMoveToEndOfLine
  ) where

import Smos.Types

import Smos.Actions.Utils

allContentsPlainActions :: [Action]
allContentsPlainActions =
  [ contentsRemove
  , contentsDelete
  , contentsMoveLeft
  , contentsMoveRight
  , contentsMoveUp
  , contentsMoveDown
  ]

allContentsUsingCharActions :: [ActionUsing Char]
allContentsUsingCharActions = [contentsInsert, contentsAppend]

contentsInsert :: ActionUsing Char
contentsInsert =
  ActionUsing
    { actionUsingName = "contentsInsert"
    , actionUsingFunc =
        \c -> modifyMContentsCursorWhenSelected (contentsCursorInsertChar c)
    , actionUsingDescription =
        "Insert a character into the contents in front of the cursor"
    }

contentsInsertNewline :: Action
contentsInsertNewline =
  Action
    { actionName = "contentsInsertNewline"
    , actionFunc = modifyMContentsCursorWhenSelected contentsCursorInsertNewline
    , actionDescription =
        "Insert a newline into the contents in front of the cursor"
    }

contentsAppend :: ActionUsing Char
contentsAppend =
  ActionUsing
    { actionUsingName = "contentsAppend"
    , actionUsingFunc =
        \c -> modifyMContentsCursorWhenSelected (contentsCursorAppendChar c)
    , actionUsingDescription =
        "Append a character into the contents in front of the cursor"
    }

contentsAppendNewline :: Action
contentsAppendNewline =
  Action
    { actionName = "contentsAppendNewline"
    , actionFunc = modifyMContentsCursorWhenSelected contentsCursorAppendNewline
    , actionDescription =
        "Append a newline into the contents in front of the cursor"
    }

contentsRemove :: Action
contentsRemove =
  Action
    { actionName = "contentsRemove"
    , actionFunc = modifyContentsCursorWhenSelectedDM contentsCursorRemove
    , actionDescription = "Remove a character from the contents"
    }

contentsDelete :: Action
contentsDelete =
  Action
    { actionName = "contentsDelete"
    , actionFunc = modifyContentsCursorWhenSelectedDM contentsCursorDelete
    , actionDescription = "Remove a character from the contents"
    }

contentsMoveLeft :: Action
contentsMoveLeft =
  Action
    { actionName = "contentsMoveLeft"
    , actionFunc =
        modifyContentsCursorWhenSelectedM contentsCursorSelectPrevChar
    , actionDescription = "Move left in the contents"
    }

contentsMoveRight :: Action
contentsMoveRight =
  Action
    { actionName = "contentsMoveRight"
    , actionFunc =
        modifyContentsCursorWhenSelectedM contentsCursorSelectNextChar
    , actionDescription = "Move right in the contents"
    }

contentsMoveUp :: Action
contentsMoveUp =
  Action
    { actionName = "contentsMoveUp"
    , actionFunc =
        modifyContentsCursorWhenSelectedM contentsCursorSelectPrevLine
    , actionDescription = "Move up in the contents"
    }

contentsMoveDown :: Action
contentsMoveDown =
  Action
    { actionName = "contentsMoveDown"
    , actionFunc =
        modifyContentsCursorWhenSelectedM contentsCursorSelectNextLine
    , actionDescription = "Move down in the contents"
    }

contentsMoveToStartOfLine :: Action
contentsMoveToStartOfLine =
  Action
    { actionName = "contentsMoveToStartOfLine"
    , actionFunc =
        modifyContentsCursorWhenSelected contentsCursorSelectStartOfLine
    , actionDescription =
        "Move to the start of the current line in the contents"
    }

contentsMoveToEndOfLine :: Action
contentsMoveToEndOfLine =
  Action
    { actionName = "contentsMoveToEndOfLine"
    , actionFunc =
        modifyContentsCursorWhenSelected contentsCursorSelectEndOfLine
    , actionDescription = "Move to the of the current line in the contents"
    }
