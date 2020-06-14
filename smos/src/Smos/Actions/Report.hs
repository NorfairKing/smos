{-# LANGUAGE OverloadedStrings #-}

module Smos.Actions.Report where

import Smos.Actions.File
import Smos.Actions.Utils
import Smos.Report.Config
import Smos.Types

allPlainReportNextActions :: [Action]
allPlainReportNextActions =
  [ reportNextActions,
    nextNextAction,
    prevNextAction,
    firstNextAction,
    lastNextAction,
    enterNextActionFile,
    selectNextActionReport,
    selectNextActionFilter
  ]

allReportNextActionsUsingActions :: [ActionUsing Char]
allReportNextActionsUsingActions =
  [insertNextActionFilter]

reportNextActions :: Action
reportNextActions =
  Action
    { actionName = "reportNextActions",
      actionFunc = modifyEditorCursorS $ \ec -> do
        saveCurrentSmosFile
        dc <- asks $ smosReportConfigDirectoryConfig . configReportConfig
        mnarc <- liftIO $ produceNextActionReportCursor dc
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
                dc <- asks $ smosReportConfigDirectoryConfig . configReportConfig
                wd <- liftIO $ resolveDirWorkflowDir dc
                void $ switchToCursor (nextActionReportCursorBuildFilePath wd narc) (Just sfc)
          _ -> pure (),
      actionDescription = "Enter the currently selected next action"
    }

insertNextActionFilter :: ActionUsing Char
insertNextActionFilter =
  ActionUsing
  { actionUsingName = "insertNextActionFilter"
  , actionUsingDescription = "Insert a character into the filter bar"
  , actionUsingFunc = \a -> modifyNextActionReportCursorM $ nextActionReportCursorInsert a
  }

appendNextActionFilter :: ActionUsing Char
appendNextActionFilter =
  ActionUsing
  { actionUsingName = "appendNextActionFilter"
  , actionUsingDescription = "Append a character onto the filter bar"
  , actionUsingFunc = \a -> modifyNextActionReportCursorM $ nextActionReportCursorAppend a
  }

removeNextActionFilter :: Action
removeNextActionFilter =
  Action
  { actionName = "removeNextActionFilter"
  , actionDescription = "Remove the character in filter bar before cursor"
  , actionFunc = modifyNextActionReportCursorM nextActionReportCursorRemove
  }

deleteNextActionFilter :: Action
deleteNextActionFilter =
  Action
  { actionName = "deleteNextActionFilter"
  , actionDescription = "Remove the character in filter bar under cursor"
  , actionFunc = modifyNextActionReportCursorM nextActionReportCursorDelete
  }

selectNextActionReport :: Action
selectNextActionReport =
  Action
  { actionName = "selectNextActionReport"
  , actionDescription = "Select the next action report"
  , actionFunc = modifyNextActionReportCursorM nextActionReportCursorSelectReport
  }

selectNextActionFilter :: Action
selectNextActionFilter =
  Action
  { actionName = "selectNextActionFilter"
  , actionDescription = "Select the next action filter bar"
  , actionFunc = modifyNextActionReportCursorM nextActionReportCursorSelectFilter
  }
