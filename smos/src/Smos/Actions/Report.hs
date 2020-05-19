{-# LANGUAGE OverloadedStrings #-}

module Smos.Actions.Report where

import Smos.Actions.File
import Smos.Actions.Utils
import Smos.Types

allPlainReportNextActions :: [Action]
allPlainReportNextActions =
  [ reportNextActions,
    nextNextAction,
    prevNextAction,
    firstNextAction,
    lastNextAction,
    enterNextActionFile
  ]

reportNextActions :: Action
reportNextActions =
  Action
    { actionName = "reportNextActions",
      actionFunc = modifyEditorCursorS $ \ec -> do
        saveCurrentSmosFile
        rc <- asks configReportConfig
        mnarc <- liftIO $ produceNextActionReportCursor rc
        pure $
          case mnarc of
            Nothing -> ec
            Just narc -> editorCursorSwitchToNextActionReport narc ec,
      actionDescription = "Next action report"
    }

nextNextAction :: Action
nextNextAction =
  Action
    { actionName = "nextNextAction",
      actionFunc = modifyNextActionReportCursorM nextActionReportCursorNext,
      actionDescription = "Select the next next action"
    }

prevNextAction :: Action
prevNextAction =
  Action
    { actionName = "prevNextAction",
      actionFunc = modifyNextActionReportCursorM nextActionReportCursorPrev,
      actionDescription = "Select the previous next action"
    }

firstNextAction :: Action
firstNextAction =
  Action
    { actionName = "firstNextAction",
      actionFunc = modifyNextActionReportCursor nextActionReportCursorFirst,
      actionDescription = "Select the first next action"
    }

lastNextAction :: Action
lastNextAction =
  Action
    { actionName = "lastNextAction",
      actionFunc = modifyNextActionReportCursor nextActionReportCursorLast,
      actionDescription = "Select the last next action"
    }

enterNextActionFile :: Action
enterNextActionFile =
  Action
    { actionName = "enterNextActionFile",
      actionFunc = do
        ss <- get
        let ec = smosStateCursor ss
        case editorCursorSelection ec of
          ReportSelected ->
            case editorCursorReportCursor ec of
              Nothing -> pure ()
              Just (ReportNextActions narc) -> do
                let sfc = nextActionReportCursorBuildSmosFileCursor narc
                void $ switchToFile (nextActionReportCursorBuildFilePath narc) sfc
          _ -> pure (),
      actionDescription = "Enter the currently selected next action"
    }
