{-# LANGUAGE OverloadedStrings #-}

module Smos.Actions.Entry.Contents
  ( allContentsPlainActions,
    allContentsUsingCharActions,
    contentsInsert,
    contentsInsertNewline,
    contentsAppend,
    contentsAppendNewline,
    contentsRemove,
    contentsDelete,
    contentsMoveLeft,
    contentsMoveRight,
    contentsMoveUp,
    contentsMoveDown,
    contentsMoveToStartOfLine,
    contentsMoveToEndOfLine,
    contentsMoveToNextWord,
    contentsMoveToPrevWord,
    contentsMoveToBeginningOfWord,
    contentsMoveToEndOfWord,
  )
where

import Smos.Actions.Utils
import Smos.Types

allContentsPlainActions :: [Action]
allContentsPlainActions =
  [ contentsInsertNewline,
    contentsAppendNewline,
    contentsRemove,
    contentsDelete,
    contentsMoveLeft,
    contentsMoveRight,
    contentsMoveUp,
    contentsMoveDown,
    contentsMoveToStartOfLine,
    contentsMoveToEndOfLine,
    contentsMoveToNextWord,
    contentsMoveToPrevWord,
    contentsMoveToBeginningOfWord,
    contentsMoveToEndOfWord
  ]

allContentsUsingCharActions :: [ActionUsing Char]
allContentsUsingCharActions = [contentsInsert, contentsAppend]

contentsInsert :: ActionUsing Char
contentsInsert =
  ActionUsing
    { actionUsingName = "contentsInsert",
      actionUsingFunc = \c -> do
        modifyMContentsCursorWhenSelectedM (contentsCursorInsertChar c)
        unrecordFileCursorHistory,
      actionUsingDescription = "Insert a character into the contents in front of the cursor"
    }

contentsInsertNewline :: Action
contentsInsertNewline =
  Action
    { actionName = "contentsInsertNewline",
      actionFunc = do
        modifyMContentsCursorWhenSelected contentsCursorInsertNewline
        unrecordFileCursorHistory,
      actionDescription = "Insert a newline into the contents in front of the cursor"
    }

contentsAppend :: ActionUsing Char
contentsAppend =
  ActionUsing
    { actionUsingName = "contentsAppend",
      actionUsingFunc = \c -> do
        modifyMContentsCursorWhenSelectedM (contentsCursorAppendChar c)
        unrecordFileCursorHistory,
      actionUsingDescription = "Append a character into the contents in front of the cursor"
    }

contentsAppendNewline :: Action
contentsAppendNewline =
  Action
    { actionName = "contentsAppendNewline",
      actionFunc = do
        modifyMContentsCursorWhenSelected contentsCursorAppendNewline
        unrecordFileCursorHistory,
      actionDescription = "Append a newline into the contents in front of the cursor"
    }

contentsRemove :: Action
contentsRemove =
  Action
    { actionName = "contentsRemove",
      actionFunc = do
        modifyContentsCursorWhenSelectedDM contentsCursorRemove
        unrecordFileCursorHistory,
      actionDescription = "Remove a character from the contents"
    }

contentsDelete :: Action
contentsDelete =
  Action
    { actionName = "contentsDelete",
      actionFunc = do
        modifyContentsCursorWhenSelectedDM contentsCursorDelete
        unrecordFileCursorHistory,
      actionDescription = "Remove a character from the contents"
    }

contentsMoveLeft :: Action
contentsMoveLeft =
  Action
    { actionName = "contentsMoveLeft",
      actionFunc = modifyContentsCursorWhenSelectedM contentsCursorSelectPrevChar,
      actionDescription = "Move left in the contents"
    }

contentsMoveRight :: Action
contentsMoveRight =
  Action
    { actionName = "contentsMoveRight",
      actionFunc = modifyContentsCursorWhenSelectedM contentsCursorSelectNextChar,
      actionDescription = "Move right in the contents"
    }

contentsMoveUp :: Action
contentsMoveUp =
  Action
    { actionName = "contentsMoveUp",
      actionFunc = modifyContentsCursorWhenSelected contentsCursorSelectPrevLineOrTheStartOfThisLine,
      actionDescription = "Move up in the contents"
    }

contentsMoveDown :: Action
contentsMoveDown =
  Action
    { actionName = "contentsMoveDown",
      actionFunc = modifyContentsCursorWhenSelected contentsCursorSelectNextLineOrTheEndOfThisLine,
      actionDescription = "Move down in the contents"
    }

contentsMoveToStartOfLine :: Action
contentsMoveToStartOfLine =
  Action
    { actionName = "contentsMoveToStartOfLine",
      actionFunc = modifyContentsCursorWhenSelected contentsCursorSelectStartOfLine,
      actionDescription = "Move to the start of the current line in the contents"
    }

contentsMoveToEndOfLine :: Action
contentsMoveToEndOfLine =
  Action
    { actionName = "contentsMoveToEndOfLine",
      actionFunc = modifyContentsCursorWhenSelected contentsCursorSelectEndOfLine,
      actionDescription = "Move to the of the current line in the contents"
    }

contentsMoveToNextWord :: Action
contentsMoveToNextWord =
  Action
    { actionName = "contentsMoveToNextWord",
      actionFunc = modifyContentsCursorWhenSelected contentsCursorSelectNextWord,
      actionDescription = "Move to the next word in the contents"
    }

contentsMoveToPrevWord :: Action
contentsMoveToPrevWord =
  Action
    { actionName = "contentsMoveToPrevWord",
      actionFunc = modifyContentsCursorWhenSelected contentsCursorSelectPrevWord,
      actionDescription = "Move to the previous word in the contents"
    }

contentsMoveToEndOfWord :: Action
contentsMoveToEndOfWord =
  Action
    { actionName = "contentsMoveToEndOfWord",
      actionFunc = modifyContentsCursorWhenSelected contentsCursorSelectEndWord,
      actionDescription = "Move to the end of the word in the contents"
    }

contentsMoveToBeginningOfWord :: Action
contentsMoveToBeginningOfWord =
  Action
    { actionName = "contentsMoveToBeginningOfWord",
      actionFunc = modifyContentsCursorWhenSelected contentsCursorSelectBeginWord,
      actionDescription = "Move to the beginning of the word in the contents"
    }
