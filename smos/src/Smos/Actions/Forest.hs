{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Smos.Actions.Forest
  ( allForestPlainActions,
    allForestUsingCharActions,
    forestToggleCollapse,
    forestToggleCollapseRecursively,
    forestToggleCollapseEntireEntry,
    forestToggleCollapseEntryContents,
    forestToggleCollapseEntryHistory,
    forestToggleCollapseEntryLogbook,
    forestToggleCollapseEntryTimestamps,
    forestToggleCollapseEntryProperties,
    forestInsertEntryBefore,
    forestInsertEntryBeforeAndSelectHeader,
    forestInsertEntryBelowAtStart,
    forestInsertEntryBelowAtStartAndSelectHeader,
    forestInsertEntryBelowAtEnd,
    forestInsertEntryBelowAtEndAndSelectHeader,
    forestInsertEntryAfter,
    forestInsertEntryAfterAndSelectHeader,
    forestDeleteCurrentEntry,
    forestDeleteCurrentSubTree,
    forestMoveUp,
    forestMoveDown,
    forestMoveLeft,
    forestMoveRight,
    forestMoveToFirst,
    forestMoveToLast,
    forestSwapUp,
    forestSwapDown,
    forestPromoteEntry,
    forestPromoteSubTree,
    forestDemoteEntry,
    forestDemoteSubTree,
    forestClockOutEverywhereInThisFile,
    forestClockOutEverywhereInAllFiles,
    forestClockOutEverywhereInThisFileAndClockInHere,
    forestClockOutEverywhereInAllFilesAndClockInHere,
  )
where

import Conduit
import qualified Data.Conduit.Combinators as C
import Data.Text (Text, toLower, toTitle)
import Data.Time
import Lens.Micro
import Path
import Smos.Actions.Utils
import Smos.Cursor.SmosFileEditor
import Smos.Data
import Smos.Report.Archive
import Smos.Report.Config
import Smos.Report.Streaming
import Smos.Types

allForestPlainActions :: [Action]
allForestPlainActions =
  [ forestToggleCollapse,
    forestToggleCollapseRecursively,
    forestToggleCollapseEntireEntry,
    forestToggleCollapseEntryContents,
    forestToggleCollapseEntryHistory,
    forestToggleCollapseEntryLogbook,
    forestToggleCollapseEntryTimestamps,
    forestToggleCollapseEntryProperties,
    forestInsertEntryBefore,
    forestInsertEntryBeforeAndSelectHeader,
    forestInsertEntryBelowAtStart,
    forestInsertEntryBelowAtStartAndSelectHeader,
    forestInsertEntryBelowAtEnd,
    forestInsertEntryBelowAtEndAndSelectHeader,
    forestInsertEntryAfter,
    forestInsertEntryAfterAndSelectHeader,
    forestDeleteCurrentEntry,
    forestDeleteCurrentSubTree,
    forestMoveUp,
    forestMoveDown,
    forestMoveLeft,
    forestMoveRight,
    forestMoveToFirst,
    forestMoveToLast,
    forestSwapUp,
    forestSwapDown,
    forestPromoteEntry,
    forestPromoteSubTree,
    forestDemoteEntry,
    forestDemoteSubTree,
    forestClockOutEverywhereInThisFile,
    forestClockOutEverywhereInAllFiles,
    forestClockOutEverywhereInThisFileAndClockInHere,
    forestClockOutEverywhereInAllFilesAndClockInHere
  ]

allForestUsingCharActions :: [ActionUsing Char]
allForestUsingCharActions = []

forestToggleCollapse :: Action
forestToggleCollapse =
  Action
    { actionName = "forestToggleCollapse",
      actionFunc = modifyFileCursorM smosFileCursorToggleCollapse,
      actionDescription = "Toggle collapsing the current sub forest"
    }

forestToggleCollapseRecursively :: Action
forestToggleCollapseRecursively =
  Action
    { actionName = "forestToggleCollapseRecursively",
      actionFunc = modifyFileCursorM smosFileCursorToggleCollapseRecursively,
      actionDescription = "Toggle collapsing the current sub forest recursively"
    }

forestInsertEntryBefore :: Action
forestInsertEntryBefore =
  Action
    { actionName = "forestInsertEntryBefore",
      actionFunc = modifyFileCursor smosFileCursorInsertEntryAfter,
      actionDescription = "Insert an entry before the currently selected header, on the same level"
    }

forestInsertEntryBeforeAndSelectHeader :: Action
forestInsertEntryBeforeAndSelectHeader =
  Action
    { actionName = "forestInsertEntryBeforeAndSelectHeader",
      actionFunc = modifyFileCursor smosFileCursorInsertEntryBeforeAndSelectHeader,
      actionDescription =
        "Insert an entry before the currently selected entry, on the same level, and select its header"
    }

forestInsertEntryBelowAtStart :: Action
forestInsertEntryBelowAtStart =
  Action
    { actionName = "forestInsertEntryBelowAtStart",
      actionFunc = modifyFileCursor smosFileCursorInsertEntryBefore,
      actionDescription = "Insert an entry below the currently selected entry at the end of the list of child nodes"
    }

forestInsertEntryBelowAtStartAndSelectHeader :: Action
forestInsertEntryBelowAtStartAndSelectHeader =
  Action
    { actionName = "forestInsertEntryBelowAtStartAndSelectHeader",
      actionFunc = modifyFileCursor smosFileCursorInsertEntryBelowAtStartAndSelectHeader,
      actionDescription = "Insert an entry below the currently selected entry at the end of the list of child nodes and select its header"
    }

forestInsertEntryBelowAtEnd :: Action
forestInsertEntryBelowAtEnd =
  Action
    { actionName = "forestInsertEntryBelowAtEnd",
      actionFunc = modifyFileCursor smosFileCursorInsertEntryBefore,
      actionDescription = "Insert an entry below the currently selected entry at the start of the list of child nodes"
    }

forestInsertEntryBelowAtEndAndSelectHeader :: Action
forestInsertEntryBelowAtEndAndSelectHeader =
  Action
    { actionName = "forestInsertEntryBelowAtEndAndSelectHeader",
      actionFunc = modifyFileCursor smosFileCursorInsertEntryBelowAtEndAndSelectHeader,
      actionDescription = "Insert an entry below the currently selected entry at the start of the list of child nodes and select its header"
    }

forestInsertEntryAfter :: Action
forestInsertEntryAfter =
  Action
    { actionName = "forestInsertEntryAfter",
      actionFunc = modifyFileCursor smosFileCursorInsertEntryAfter,
      actionDescription = "Insert an entry after the currently selected entry, on the same level"
    }

forestInsertEntryAfterAndSelectHeader :: Action
forestInsertEntryAfterAndSelectHeader =
  Action
    { actionName = "forestInsertEntryAfterAndSelectHeader",
      actionFunc = modifyFileCursor smosFileCursorInsertEntryAfterAndSelectHeader,
      actionDescription =
        "Insert an entry after the currently selected entry, on the same level, and select its header"
    }

forestDeleteCurrentEntry :: Action
forestDeleteCurrentEntry =
  Action
    { actionName = "forestDeleteCurrentEntry",
      actionFunc = modifyFileCursorD smosFileCursorDeleteElem,
      actionDescription = "Delete the current entry"
    }

forestDeleteCurrentSubTree :: Action
forestDeleteCurrentSubTree =
  Action
    { actionName = "forestDeleteCurrentSubTree",
      actionFunc = modifyFileCursorD smosFileCursorDeleteSubTree,
      actionDescription = "Delete the current entry and all entries below"
    }

forestMoveUp :: Action
forestMoveUp =
  Action
    { actionName = "forestMoveUp",
      actionFunc = modifyFileCursorM smosFileCursorSelectPrev,
      actionDescription = "Move the current cursor up to the previous entry in the entry forest"
    }

forestMoveDown :: Action
forestMoveDown =
  Action
    { actionName = "forestMoveDown",
      actionFunc = modifyFileCursorM smosFileCursorSelectNext,
      actionDescription = "Move the current cursor down to the previous entry in the entry forest"
    }

forestMoveLeft :: Action
forestMoveLeft =
  Action
    { actionName = "forestMoveLeft",
      actionFunc = modifyFileCursorM smosFileCursorSelectAbove,
      actionDescription = "Move the current cursor to the parent entry in the entry forest"
    }

forestMoveRight :: Action
forestMoveRight =
  Action
    { actionName = "forestMoveRight",
      actionFunc = modifyFileCursorM smosFileCursorSelectBelowAtEnd,
      actionDescription =
        "Move the current cursor to the first child of the current entry in the entry forest"
    }

forestMoveToFirst :: Action
forestMoveToFirst =
  Action
    { actionName = "forestMoveToFirst",
      actionFunc = modifyFileCursor smosFileCursorSelectFirst,
      actionDescription = "Move the current cursor up to the first entry in the entry forest"
    }

forestMoveToLast :: Action
forestMoveToLast =
  Action
    { actionName = "forestMoveToLast",
      actionFunc = modifyFileCursor smosFileCursorSelectLast,
      actionDescription = "Move the current cursor down to the last entry in the entry forest"
    }

forestSwapUp :: Action
forestSwapUp =
  Action
    { actionName = "forestSwapUp",
      actionFunc = modifyFileCursorM smosFileCursorSwapPrev,
      actionDescription = "Swap the current and the previous entry on the same level."
    }

forestSwapDown :: Action
forestSwapDown =
  Action
    { actionName = "forestSwapDown",
      actionFunc = modifyFileCursorM smosFileCursorSwapNext,
      actionDescription = "Swap the current and the next entry on the same level."
    }

forestPromoteEntry :: Action
forestPromoteEntry =
  Action
    { actionName = "forestPromoteEntry",
      actionFunc = modifyFileCursorM smosFileCursorPromoteEntry,
      actionDescription = "Promotes the current entry"
    }

forestPromoteSubTree :: Action
forestPromoteSubTree =
  Action
    { actionName = "forestPromoteSubTree",
      actionFunc = modifyFileCursorM smosFileCursorPromoteSubTree,
      actionDescription = "Promotes the current sub tree"
    }

forestDemoteEntry :: Action
forestDemoteEntry =
  Action
    { actionName = "forestDemoteEntry",
      actionFunc = modifyFileCursorM smosFileCursorDemoteEntry,
      actionDescription = "Demotes the current entry"
    }

forestDemoteSubTree :: Action
forestDemoteSubTree =
  Action
    { actionName = "forestDemoteSubTree",
      actionFunc = modifyFileCursorM smosFileCursorDemoteSubTree,
      actionDescription = "Demotes the current sub tree"
    }

forestToggleCollapseEntireEntry :: Action
forestToggleCollapseEntireEntry =
  Action
    { actionName = "forestToggleCollapseEntireEntry",
      actionFunc = modifyFileCursor smosFileCursorToggleCollapseEntireEntry,
      actionDescription = "Toggle the hiding of the current entire entry"
    }

mkForestToggleCollapseEntryPart :: Text -> Lens' (CollapseEntry EntryCursor) Bool -> Action
mkForestToggleCollapseEntryPart name field =
  Action
    { actionName = "forestToggleCollapseEntry" <> ActionName (toTitle name),
      actionFunc = modifyFileCursor $ smosFileCursorToggleCollapseEntryLens field,
      actionDescription = "Toggle the hiding of the " <> toLower name <> " of the current entry"
    }

forestToggleCollapseEntryContents :: Action
forestToggleCollapseEntryContents = mkForestToggleCollapseEntryPart "contents" collapseEntryShowContentsL

forestToggleCollapseEntryHistory :: Action
forestToggleCollapseEntryHistory = mkForestToggleCollapseEntryPart "history" collapseEntryShowHistoryL

forestToggleCollapseEntryLogbook :: Action
forestToggleCollapseEntryLogbook = mkForestToggleCollapseEntryPart "logbook" collapseEntryShowLogbookL

forestToggleCollapseEntryTimestamps :: Action
forestToggleCollapseEntryTimestamps = mkForestToggleCollapseEntryPart "timestamps" collapseEntryShowTimestampsL

forestToggleCollapseEntryProperties :: Action
forestToggleCollapseEntryProperties = mkForestToggleCollapseEntryPart "properties" collapseEntryShowPropertiesL

forestClockOutEverywhereInThisFile :: Action
forestClockOutEverywhereInThisFile =
  Action
    { actionName = "forestClockOutEverywhereInThisFile",
      actionFunc = modifyFileCursorS $ \sfc -> do
        now <- liftIO getCurrentTime
        pure $ smosFileCursorClockOutEverywhere now sfc,
      actionDescription = "Clock out everywhere in this file"
    }

forestClockOutEverywhereInAllFiles :: Action
forestClockOutEverywhereInAllFiles =
  Action
    { actionName = "forestClockOutEverywhereInAllFiles",
      actionFunc = do
        now <- liftIO getCurrentTime
        clockOutInAllAgendaFiles now,
      actionDescription = "Clock out everywhere in all files"
    }

forestClockOutEverywhereInThisFileAndClockInHere :: Action
forestClockOutEverywhereInThisFileAndClockInHere =
  Action
    { actionName = "forestClockOutEverywhereInThisFileAndClockInHere",
      actionFunc = modifyFileCursorS $ \sfc -> do
        now <- liftIO getCurrentTime
        pure $ smosFileCursorClockOutEverywhereAndClockInHere now sfc,
      actionDescription = "Clock out everywhere in this file and clock in at the current entry"
    }

forestClockOutEverywhereInAllFilesAndClockInHere :: Action
forestClockOutEverywhereInAllFilesAndClockInHere =
  Action
    { actionName = "forestClockOutEverywhereInAllFilesAndClockInHere",
      actionFunc = do
        now <- liftIO getCurrentTime
        clockOutInAllAgendaFiles now
        modifyFileCursorS $ \sfc -> pure $ smosFileCursorClockOutEverywhereAndClockInHere now sfc,
      actionDescription = "Clock out everywhere in all files and clock in at the current entry"
    }

clockOutInAllAgendaFiles :: UTCTime -> SmosM ()
clockOutInAllAgendaFiles now = do
  dirConfig <- asks $ smosReportConfigDirectoryConfig . configReportConfig
  mCurFile <- gets $ fmap smosFileEditorPath . editorCursorFileCursor . smosStateCursor
  -- We won't clock out in the current file asynchronously because this produces a race condition.
  let isCurrent af =
        case mCurFile of
          Nothing -> False
          Just cur -> cur == af
  runSmosAsync $ do
    wd <- resolveDirWorkflowDir dirConfig
    let clockOutSingle rp = do
          let af = wd </> rp
          unless (isCurrent af) $ do
            merrOrFile <- readSmosFile af
            case merrOrFile of
              Nothing -> pure () -- Should not happen
              Just (Left _) -> pure () -- Nothing we can do
              Just (Right sf) -> do
                let sf' = smosFileClockOutEverywhere now sf
                unless (sf == sf') $ writeSmosFile af sf'
    runConduit $ streamSmosFilesFromWorkflowRel HideArchive dirConfig .| C.mapM_ clockOutSingle
  modifyFileCursorS $ \sfc -> pure $ smosFileCursorClockOutEverywhere now sfc
