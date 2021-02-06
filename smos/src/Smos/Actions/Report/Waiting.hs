{-# LANGUAGE OverloadedStrings #-}

module Smos.Actions.Report.Waiting where

import Smos.Actions.File
import Smos.Actions.Utils
import Smos.Report.Archive
import Smos.Report.Config
import Smos.Report.ShouldPrint
import Smos.Types

allPlainReportWaitingActions :: [Action]
allPlainReportWaitingActions =
  [ reportWaiting,
    enterWaitingFile,
    prevWaiting,
    nextWaiting,
    firstWaiting,
    lastWaiting
  ]

allReportWaitingUsingActions :: [ActionUsing Char]
allReportWaitingUsingActions = []

reportWaiting :: Action
reportWaiting =
  Action
    { actionName = "reportWaiting",
      actionFunc = modifyEditorCursorS $ \ec -> do
        saveCurrentSmosFile
        dc <- asks $ smosReportConfigDirectoryConfig . configReportConfig
        narc <- liftIO $ produceWaitingReportCursor Nothing HideArchive DontPrint dc
        pure $
          ec
            { editorCursorSelection = ReportSelected,
              editorCursorReportCursor = Just $ ReportWaiting narc
            },
      actionDescription = "Waiting report"
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
              dc <- asks $ smosReportConfigDirectoryConfig . configReportConfig
              wd <- liftIO $ resolveDirWorkflowDir dc
              case waitingReportCursorBuildSmosFileCursor wd wrc of
                Nothing -> pure ()
                Just (fp, sfc) -> void $ switchToCursor fp (Just sfc)
            _ -> pure ()
          Nothing -> pure (),
      actionDescription = "Enter the currently selected waiting entry"
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
