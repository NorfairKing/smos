{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Smos.Actions.Forest
    ( allForestPlainActions
    , allForestUsingCharActions
    , forestToggleCollapse
    , forestInsertEntryBefore
    , forestInsertEntryBeforeAndSelectHeader
    , forestInsertEntryBelow
    , forestInsertEntryBelowAndSelectHeader
    , forestInsertEntryAfter
    , forestInsertEntryAfterAndSelectHeader
    , forestDeleteCurrentEntry
    , forestDeleteCurrentSubTree
    , forestMoveUp
    , forestMoveDown
    , forestMoveToFirst
    , forestMoveToLast
    , forestSwapUp
    , forestSwapDown
    , forestPromoteEntry
    , forestPromoteSubTree
    , forestDemoteEntry
    , forestDemoteSubTree
    , forestToggleHideEntireEntry
    ) where

import Smos.Types

import Smos.Actions.Utils

allForestPlainActions :: [Action]
allForestPlainActions =
    [ forestToggleCollapse
    , forestInsertEntryBefore
    , forestInsertEntryBeforeAndSelectHeader
    , forestInsertEntryBelow
    , forestInsertEntryBelowAndSelectHeader
    , forestInsertEntryAfter
    , forestInsertEntryAfterAndSelectHeader
    , forestDeleteCurrentEntry
    , forestDeleteCurrentSubTree
    , forestMoveUp
    , forestMoveDown
    , forestSwapUp
    , forestSwapDown
    , forestPromoteEntry
    , forestPromoteSubTree
    , forestDemoteEntry
    , forestDemoteSubTree
    , forestToggleHideEntireEntry
    ]

allForestUsingCharActions :: [ActionUsing Char]
allForestUsingCharActions = []

forestToggleCollapse :: Action
forestToggleCollapse =
    Action
    { actionName = "forestToggleCollapse"
    , actionFunc = modifyFileCursorM smosFileCursorToggleCollapse
    , actionDescription = "Toggle collapsing the current sub forest"
    }

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

forestInsertEntryBelow :: Action
forestInsertEntryBelow =
    Action
    { actionName = "forestInsertEntryBelow"
    , actionFunc = modifyFileCursor smosFileCursorInsertEntryBefore
    , actionDescription = "Insert an entry below the currently selected entry"
    }

forestInsertEntryBelowAndSelectHeader :: Action
forestInsertEntryBelowAndSelectHeader =
    Action
    { actionName = "forestInsertEntryBelowAndSelectHeader"
    , actionFunc =
          modifyFileCursor smosFileCursorInsertEntryBelowAndSelectHeader
    , actionDescription = "Insert an entry below the currently selected entry"
    }

forestInsertEntryAfter :: Action
forestInsertEntryAfter =
    Action
    { actionName = "forestInsertEntryAfter"
    , actionFunc = modifyFileCursor smosFileCursorInsertEntryAfter
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

forestDeleteCurrentEntry :: Action
forestDeleteCurrentEntry =
    Action
    { actionName = "forestDeleteCurrentEntry"
    , actionFunc = modifyFileCursorD smosFileCursorDeleteElem
    , actionDescription = "Delete the current entry"
    }

forestDeleteCurrentSubTree :: Action
forestDeleteCurrentSubTree =
    Action
    { actionName = "forestDeleteCurrentSubTree"
    , actionFunc = modifyFileCursorD smosFileCursorDeleteSubTree
    , actionDescription = "Delete the current entry and all entries below"
    }

forestMoveUp :: Action
forestMoveUp =
    Action
    { actionName = "forestMoveUp"
    , actionFunc = modifyFileCursorM smosFileCursorSelectPrev
    , actionDescription =
          "Move the current cursor up to the previous entry in the entry forest"
    }

forestMoveDown :: Action
forestMoveDown =
    Action
    { actionName = "forestMoveDown"
    , actionFunc = modifyFileCursorM smosFileCursorSelectNext
    , actionDescription =
          "Move the current cursor down to the previous entry in the entry forest"
    }

forestMoveToFirst :: Action
forestMoveToFirst =
    Action
    { actionName = "forestMoveToFirst"
    , actionFunc = modifyFileCursor smosFileCursorSelectFirst
    , actionDescription =
          "Move the current cursor up to the first entry in the entry forest"
    }

forestMoveToLast :: Action
forestMoveToLast =
    Action
    { actionName = "forestMoveToLast"
    , actionFunc = modifyFileCursor smosFileCursorSelectLast
    , actionDescription =
          "Move the current cursor down to the last entry in the entry forest"
    }

forestSwapUp :: Action
forestSwapUp =
    Action
    { actionName = "forestSwapUp"
    , actionFunc = modifyFileCursorM smosFileCursorSwapPrev
    , actionDescription =
          "Swap the current and the previous entry on the same level."
    }

forestSwapDown :: Action
forestSwapDown =
    Action
    { actionName = "forestSwapDown"
    , actionFunc = modifyFileCursorM smosFileCursorSwapNext
    , actionDescription =
          "Swap the current and the next entry on the same level."
    }

forestPromoteEntry :: Action
forestPromoteEntry =
    Action
    { actionName = "forestPromoteEntry"
    , actionFunc = modifyFileCursorM smosFileCursorPromoteEntry
    , actionDescription = "Promotes the current entry"
    }

forestPromoteSubTree :: Action
forestPromoteSubTree =
    Action
    { actionName = "forestPromoteSubTree"
    , actionFunc = modifyFileCursorM smosFileCursorPromoteSubTree
    , actionDescription = "Promotes the current sub tree"
    }

forestDemoteEntry :: Action
forestDemoteEntry =
    Action
    { actionName = "forestDemoteEntry"
    , actionFunc = modifyFileCursorM smosFileCursorDemoteEntry
    , actionDescription = "Demotes the current entry"
    }

forestDemoteSubTree :: Action
forestDemoteSubTree =
    Action
    { actionName = "forestDemoteSubTree"
    , actionFunc = modifyFileCursorM smosFileCursorDemoteSubTree
    , actionDescription = "Demotes the current sub tree"
    }

forestToggleHideEntireEntry :: Action
forestToggleHideEntireEntry =
    Action
    { actionName = "forestToggleHideEntireEntry"
    , actionFunc = modifyFileCursor smosFileCursorToggleHideEntireEntry
    , actionDescription = "Toggle the hiding of the current entire entry"
    }
