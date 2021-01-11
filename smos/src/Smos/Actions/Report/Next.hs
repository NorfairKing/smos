{-# LANGUAGE OverloadedStrings #-}

module Smos.Actions.Report.Next where

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
    selectNextActionFilter,
    removeNextActionFilter,
    deleteNextActionFilter
  ]

allReportNextActionsUsingActions :: [ActionUsing Char]
allReportNextActionsUsingActions =
  [ insertNextActionFilter,
    appendNextActionFilter
  ]

reportNextActions :: Action
reportNextActions =
  Action
    { actionName = "reportNextActions",
      actionFunc = modifyEditorCursorS $ \ec -> do
        saveCurrentSmosFile
        dc <- asks $ smosReportConfigDirectoryConfig . configReportConfig
        narc <- liftIO $ produceNextActionReportCursor dc
        pure $
          ec
            { editorCursorSelection = ReportSelected,
              editorCursorReportCursor = Just $ ReportNextActions narc
            },
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
        case editorCursorReportCursor $ smosStateCursor ss of
          Just rc -> case rc of
            ReportNextActions narc -> do
              dc <- asks $ smosReportConfigDirectoryConfig . configReportConfig
              wd <- liftIO $ resolveDirWorkflowDir dc
              case nextActionReportCursorBuildSmosFileCursor wd narc of
                Nothing -> pure ()
                Just (fp, sfc) -> void $ switchToCursor fp (Just sfc)
            _ -> pure ()
          Nothing -> pure (),
      actionDescription = "Enter the currently selected next action"
    }

insertNextActionFilter :: ActionUsing Char
insertNextActionFilter =
  ActionUsing
    { actionUsingName = "insertNextActionFilter",
      actionUsingDescription = "Insert a character into the filter bar",
      actionUsingFunc = \a -> modifyNextActionReportCursorM $ nextActionReportCursorInsert a
    }

appendNextActionFilter :: ActionUsing Char
appendNextActionFilter =
  ActionUsing
    { actionUsingName = "appendNextActionFilter",
      actionUsingDescription = "Append a character onto the filter bar",
      actionUsingFunc = \a -> modifyNextActionReportCursorM $ nextActionReportCursorAppend a
    }

removeNextActionFilter :: Action
removeNextActionFilter =
  Action
    { actionName = "removeNextActionFilter",
      actionDescription = "Remove the character in filter bar before cursor",
      actionFunc = modifyNextActionReportCursorM nextActionReportCursorRemove
    }

deleteNextActionFilter :: Action
deleteNextActionFilter =
  Action
    { actionName = "deleteNextActionFilter",
      actionDescription = "Remove the character in filter bar under cursor",
      actionFunc = modifyNextActionReportCursorM nextActionReportCursorDelete
    }

selectNextActionReport :: Action
selectNextActionReport =
  Action
    { actionName = "selectNextActionReport",
      actionDescription = "Select the next action report",
      actionFunc = modifyNextActionReportCursorM nextActionReportCursorSelectReport
    }

selectNextActionFilter :: Action
selectNextActionFilter =
  Action
    { actionName = "selectNextActionFilter",
      actionDescription = "Select the next action filter bar",
      actionFunc = modifyNextActionReportCursorM nextActionReportCursorSelectFilter
    }
