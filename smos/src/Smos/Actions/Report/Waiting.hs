{-# LANGUAGE OverloadedStrings #-}

module Smos.Actions.Report.Waiting where

import Smos.Actions.File
import Smos.Actions.Utils
import Smos.Directory.Config
import Smos.Report.Archive
import Smos.Report.Config
import Smos.Report.ShouldPrint
import Smos.Types

allPlainReportWaitingActions :: [Action]
allPlainReportWaitingActions =
  [ reportWaiting,
    prevWaiting,
    nextWaiting,
    firstWaiting,
    lastWaiting,
    enterWaitingFile,
    selectWaitingReport,
    selectWaitingFilter,
    removeWaitingFilter,
    deleteWaitingFilter
  ]

allReportWaitingUsingActions :: [ActionUsing Char]
allReportWaitingUsingActions =
  [ insertWaitingFilter,
    appendWaitingFilter
  ]

reportWaiting :: Action
reportWaiting =
  Action
    { actionName = "reportWaiting",
      actionFunc = modifyEditorCursorS $ \ec -> do
        saveCurrentSmosFile
        dc <- asks $ smosReportSettingDirectorySettings . configReportConfig
        narc <- liftIO $ produceWaitingReportCursor Nothing HideArchive DontPrint dc
        pure $
          ec
            { editorCursorSelection = ReportSelected,
              editorCursorReportCursor = Just $ ReportWaiting narc
            },
      actionDescription = "Waiting report"
    }

prevWaiting :: Action
prevWaiting =
  Action
    { actionName = "prevWaiting",
      actionFunc = modifyWaitingReportCursorM waitingReportCursorPrev,
      actionDescription = "Select the previous entry in the waiting report"
    }

nextWaiting :: Action
nextWaiting =
  Action
    { actionName = "nextWaiting",
      actionFunc = modifyWaitingReportCursorM waitingReportCursorNext,
      actionDescription = "Select the next entry in the waiting report"
    }

firstWaiting :: Action
firstWaiting =
  Action
    { actionName = "firstWaiting",
      actionFunc = modifyWaitingReportCursor waitingReportCursorFirst,
      actionDescription = "Select the first entry in the waiting report"
    }

lastWaiting :: Action
lastWaiting =
  Action
    { actionName = "lastWaiting",
      actionFunc = modifyWaitingReportCursor waitingReportCursorLast,
      actionDescription = "Select the last entry in the waiting report"
    }

enterWaitingFile :: Action
enterWaitingFile =
  Action
    { actionName = "enterWaitingFile",
      actionFunc = do
        ss <- get
        case editorCursorReportCursor $ smosStateCursor ss of
          Just rc -> case rc of
            ReportWaiting wrc -> do
              dc <- asks $ smosReportSettingDirectorySettings . configReportConfig
              wd <- liftIO $ resolveDirWorkflowDir dc
              case waitingReportCursorBuildSmosFileCursor wd wrc of
                Nothing -> pure ()
                Just (fp, sfc) -> void $ switchToCursor fp (Just sfc)
            _ -> pure ()
          Nothing -> pure (),
      actionDescription = "Enter the currently selected waiting entry"
    }

insertWaitingFilter :: ActionUsing Char
insertWaitingFilter =
  ActionUsing
    { actionUsingName = "insertWaitingFilter",
      actionUsingDescription = "Insert a character into the filter bar",
      actionUsingFunc = \a -> modifyWaitingReportCursorM $ waitingReportCursorInsert a
    }

appendWaitingFilter :: ActionUsing Char
appendWaitingFilter =
  ActionUsing
    { actionUsingName = "appendWaitingFilter",
      actionUsingDescription = "Append a character onto the filter bar",
      actionUsingFunc = \a -> modifyWaitingReportCursorM $ waitingReportCursorAppend a
    }

removeWaitingFilter :: Action
removeWaitingFilter =
  Action
    { actionName = "removeWaitingFilter",
      actionDescription = "Remove the character in filter bar before cursor",
      actionFunc = modifyWaitingReportCursorM waitingReportCursorRemove
    }

deleteWaitingFilter :: Action
deleteWaitingFilter =
  Action
    { actionName = "deleteWaitingFilter",
      actionDescription = "Remove the character in filter bar under cursor",
      actionFunc = modifyWaitingReportCursorM waitingReportCursorDelete
    }

selectWaitingReport :: Action
selectWaitingReport =
  Action
    { actionName = "selectWaitingReport",
      actionDescription = "Select the waiting report",
      actionFunc = modifyWaitingReportCursorM waitingReportCursorSelectReport
    }

selectWaitingFilter :: Action
selectWaitingFilter =
  Action
    { actionName = "selectWaitingFilter",
      actionDescription = "Select the waiting filter bar",
      actionFunc = modifyWaitingReportCursorM waitingReportCursorSelectFilter
    }
