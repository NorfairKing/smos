{-# LANGUAGE OverloadedStrings #-}

module Smos.Actions.Entry
  ( allEntryPlainActions,
    allEntryUsingCharActions,
    entrySelectWhole,
    entrySelectHeaderAtStart,
    entrySelectHeaderAtEnd,
    entrySelectContentsAtStart,
    entrySelectContentsAtEnd,
    entrySelectProperties,
    entrySelectTimestamps,
    entrySelectStateHistory,
    entrySelectTagsFromStart,
    entrySelectTagsFromBack,
    entrySelectLogbook,
    module Smos.Actions.Entry.TodoState,
  )
where

import Lens.Micro
import Smos.Actions.Entry.TodoState
import Smos.Actions.Utils
import Smos.Types

allEntryPlainActions :: [Action]
allEntryPlainActions =
  [ entrySelectWhole,
    entrySelectHeaderAtStart,
    entrySelectHeaderAtEnd,
    entrySelectContentsAtStart,
    entrySelectContentsAtEnd,
    entrySelectProperties,
    entrySelectTimestamps,
    entrySelectStateHistory,
    entrySelectTagsFromStart,
    entrySelectTagsFromBack,
    entrySelectLogbook
  ]
    ++ allTodoStatePlainActions

allEntryUsingCharActions :: [ActionUsing Char]
allEntryUsingCharActions = allTodoStateUsingCharActions

entrySelectWhole :: Action
entrySelectWhole =
  Action
    { actionName = "entrySelectWhole",
      actionFunc = modifyEntryCursor entryCursorSelectWhole,
      actionDescription = "Select the whole current Entry"
    }

entrySelectHeaderAtStart :: Action
entrySelectHeaderAtStart =
  Action
    { actionName = "entrySelectHeaderAtStart",
      actionFunc = modifyEntryCursor entryCursorSelectHeaderAtStart,
      actionDescription = "Select the current Entry's header and select the start"
    }

entrySelectHeaderAtEnd :: Action
entrySelectHeaderAtEnd =
  Action
    { actionName = "entrySelectHeaderAtEnd",
      actionFunc = modifyEntryCursor entryCursorSelectHeaderAtEnd,
      actionDescription = "Select the current Entry's header and select the end"
    }

entrySelectContentsAtStart :: Action
entrySelectContentsAtStart =
  Action
    { actionName = "entrySelectContentsAtStart",
      actionFunc = modifyCollapseEntryCursor (fmap entryCursorSelectContentsAtStart . (collapseEntryShowContentsL .~ True)),
      actionDescription = "Select the current Entry's contents at the start"
    }

entrySelectContentsAtEnd :: Action
entrySelectContentsAtEnd =
  Action
    { actionName = "entrySelectContents",
      actionFunc = modifyCollapseEntryCursor (fmap entryCursorSelectContentsAtEnd . (collapseEntryShowContentsL .~ True)),
      actionDescription = "Select the current Entry's contents at the end"
    }

entrySelectTimestamps :: Action
entrySelectTimestamps =
  Action
    { actionName = "entrySelectTimestamps",
      actionFunc = modifyEntryCursor entryCursorSelectTimestamps,
      actionDescription = "Select the current Entry's timestamps"
    }

entrySelectProperties :: Action
entrySelectProperties =
  Action
    { actionName = "entrySelectProperties",
      actionFunc = modifyEntryCursor entryCursorSelectProperties,
      actionDescription = "Select the current Entry's properties"
    }

entrySelectStateHistory :: Action
entrySelectStateHistory =
  Action
    { actionName = "entrySelectStateHistory",
      actionFunc = modifyEntryCursor entryCursorSelectStateHistory,
      actionDescription = "Select the current Entry's state history"
    }

entrySelectTagsFromStart :: Action
entrySelectTagsFromStart =
  Action
    { actionName = "entrySelectTagsFromStart",
      actionFunc = do
        modifyEntryCursor entryCursorSelectTags
        modifyMTagsCursor $ maybe (singletonTagsCursor "") $ tagsCursorSelectStartInSelectedTag . tagsCursorSelectFirstTag,
      actionDescription = "Select the current Entry's tags"
    }

entrySelectTagsFromBack :: Action
entrySelectTagsFromBack =
  Action
    { actionName = "entrySelectTagsFromBack",
      actionFunc = do
        modifyEntryCursor entryCursorSelectTags
        modifyMTagsCursor $ maybe (singletonTagsCursor "") $ tagsCursorSelectEndInSelectedTag . tagsCursorSelectLastTag,
      actionDescription = "Select the current Entry's tags from the back"
    }

entrySelectLogbook :: Action
entrySelectLogbook =
  Action
    { actionName = "entrySelectLogbook",
      actionFunc = modifyEntryCursor entryCursorSelectLogbook,
      actionDescription = "Select the current Entry's logbook"
    }
