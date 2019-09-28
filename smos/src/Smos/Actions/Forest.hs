{-# LANGUAGE OverloadedStrings #-}

module Smos.Actions.Forest
  ( allForestPlainActions
  , allForestUsingCharActions
  , forestToggleCollapse
  , forestToggleCollapseRecursively
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
  , forestMoveLeft
  , forestMoveRight
  , forestMoveToFirst
  , forestMoveToLast
  , forestSwapUp
  , forestSwapDown
  , forestPromoteEntry
  , forestPromoteSubTree
  , forestDemoteEntry
  , forestDemoteSubTree
  , forestToggleHideEntireEntry
  , forestClockOutEverywhereInThisFile
  , forestClockOutEverywhereInAllFiles
  , forestClockOutEverywhereInThisFileAndClockInHere
  , forestClockOutEverywhereInAllFilesAndClockInHere
  ) where

import Data.Time

import Conduit

import Smos.Data

import Smos.Report.Config
import Smos.Report.Path
import Smos.Report.Streaming

import Smos.Types

import Smos.Actions.Utils

allForestPlainActions :: [Action]
allForestPlainActions =
  [ forestToggleCollapse
  , forestToggleCollapseRecursively
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
  , forestMoveLeft
  , forestMoveRight
  , forestSwapUp
  , forestSwapDown
  , forestPromoteEntry
  , forestPromoteSubTree
  , forestDemoteEntry
  , forestDemoteSubTree
  , forestToggleHideEntireEntry
  , forestClockOutEverywhereInThisFile
  , forestClockOutEverywhereInAllFiles
  , forestClockOutEverywhereInThisFileAndClockInHere
  , forestClockOutEverywhereInAllFilesAndClockInHere
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

forestToggleCollapseRecursively :: Action
forestToggleCollapseRecursively =
  Action
    { actionName = "forestToggleCollapseRecursively"
    , actionFunc = modifyFileCursorM smosFileCursorToggleCollapseRecursively
    , actionDescription = "Toggle collapsing the current sub forest recursively"
    }

forestInsertEntryBefore :: Action
forestInsertEntryBefore =
  Action
    { actionName = "forestInsertEntryBefore"
    , actionFunc = modifyFileCursor smosFileCursorInsertEntryAfter
    , actionDescription = "Insert an entry before the currently selected header, on the same level"
    }

forestInsertEntryBeforeAndSelectHeader :: Action
forestInsertEntryBeforeAndSelectHeader =
  Action
    { actionName = "forestInsertEntryBeforeAndSelectHeader"
    , actionFunc = modifyFileCursor smosFileCursorInsertEntryBeforeAndSelectHeader
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
    , actionFunc = modifyFileCursor smosFileCursorInsertEntryBelowAndSelectHeader
    , actionDescription = "Insert an entry below the currently selected entry"
    }

forestInsertEntryAfter :: Action
forestInsertEntryAfter =
  Action
    { actionName = "forestInsertEntryAfter"
    , actionFunc = modifyFileCursor smosFileCursorInsertEntryAfter
    , actionDescription = "Insert an entry after the currently selected entry, on the same level"
    }

forestInsertEntryAfterAndSelectHeader :: Action
forestInsertEntryAfterAndSelectHeader =
  Action
    { actionName = "forestInsertEntryAfterAndSelectHeader"
    , actionFunc = modifyFileCursor smosFileCursorInsertEntryAfterAndSelectHeader
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
    , actionDescription = "Move the current cursor up to the previous entry in the entry forest"
    }

forestMoveDown :: Action
forestMoveDown =
  Action
    { actionName = "forestMoveDown"
    , actionFunc = modifyFileCursorM smosFileCursorSelectNext
    , actionDescription = "Move the current cursor down to the previous entry in the entry forest"
    }

forestMoveLeft :: Action
forestMoveLeft =
  Action
    { actionName = "forestMoveLeft"
    , actionFunc = modifyFileCursorM smosFileCursorSelectAbove
    , actionDescription = "Move the current cursor to the parent entry in the entry forest"
    }

forestMoveRight :: Action
forestMoveRight =
  Action
    { actionName = "forestMoveRight"
    , actionFunc = modifyFileCursorM smosFileCursorSelectBelowAtEnd
    , actionDescription =
        "Move the current cursor to the first child of the current entry in the entry forest"
    }

forestMoveToFirst :: Action
forestMoveToFirst =
  Action
    { actionName = "forestMoveToFirst"
    , actionFunc = modifyFileCursor smosFileCursorSelectFirst
    , actionDescription = "Move the current cursor up to the first entry in the entry forest"
    }

forestMoveToLast :: Action
forestMoveToLast =
  Action
    { actionName = "forestMoveToLast"
    , actionFunc = modifyFileCursor smosFileCursorSelectLast
    , actionDescription = "Move the current cursor down to the last entry in the entry forest"
    }

forestSwapUp :: Action
forestSwapUp =
  Action
    { actionName = "forestSwapUp"
    , actionFunc = modifyFileCursorM smosFileCursorSwapPrev
    , actionDescription = "Swap the current and the previous entry on the same level."
    }

forestSwapDown :: Action
forestSwapDown =
  Action
    { actionName = "forestSwapDown"
    , actionFunc = modifyFileCursorM smosFileCursorSwapNext
    , actionDescription = "Swap the current and the next entry on the same level."
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

forestClockOutEverywhereInThisFile :: Action
forestClockOutEverywhereInThisFile =
  Action
    { actionName = "forestClockOutEverywhereInThisFile"
    , actionFunc =
        modifyFileCursorS $ \sfc -> do
          now <- liftIO getCurrentTime
          pure $ smosFileCursorClockOutEverywhere now sfc
    , actionDescription = "Clock out everywhere in this file"
    }

forestClockOutEverywhereInAllFiles :: Action
forestClockOutEverywhereInAllFiles =
  Action
    { actionName = "forestClockOutEverywhereInAllFiles"
    , actionFunc =
        modifyFileCursorS $ \sfc -> do
          now <- liftIO getCurrentTime
          pure $ smosFileCursorClockOutEverywhere now sfc
    , actionDescription = "Clock out everywhere in all files"
    }

forestClockOutEverywhereInThisFileAndClockInHere :: Action
forestClockOutEverywhereInThisFileAndClockInHere =
  Action
    { actionName = "forestClockOutEverywhereInThisFileAndClockInHere"
    , actionFunc =
        modifyFileCursorS $ \sfc -> do
          now <- liftIO getCurrentTime
          clockOutInAllAgendaFiles now
          pure $ smosFileCursorClockOutEverywhereAndClockInHere now sfc
    , actionDescription = "Clock out everywhere in this file and clock in at the current entry"
    }

forestClockOutEverywhereInAllFilesAndClockInHere :: Action
forestClockOutEverywhereInAllFilesAndClockInHere =
  Action
    { actionName = "forestClockOutEverywhereInAllFilesAndClockInHere"
    , actionFunc =
        modifyFileCursorS $ \sfc -> do
          now <- liftIO getCurrentTime
          clockOutInAllAgendaFiles now
          pure $ smosFileCursorClockOutEverywhereAndClockInHere now sfc
    , actionDescription = "Clock out everywhere in all files and clock in at the current entry"
    }

clockOutInAllAgendaFiles :: UTCTime -> SmosM ()
clockOutInAllAgendaFiles now = do
  agendaFileSpec <- asks $ smosReportConfigWorkflowFileSpec . configReportConfig
  runSmosAsync $ do
    agendaFileDir <- resolveWorkflowDir agendaFileSpec
    agendaFiles <- sourceToList $ sourceFilesInNonHiddenDirsRecursively agendaFileDir
    forM_ agendaFiles $ \rp -> do
      let af = resolveRootedPath rp
      merrOrFile <- readSmosFile af
      case merrOrFile of
        Nothing -> pure () -- Should not happen
        Just (Left _) -> pure () -- Nothing we can do
        Just (Right sf) -> do
          let sf' = smosFileClockOutEverywhere now sf
          unless (sf == sf') $ writeSmosFile af sf'
