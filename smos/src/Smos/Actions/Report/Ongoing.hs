{-# LANGUAGE OverloadedStrings #-}

module Smos.Actions.Report.Ongoing where

import Smos.Actions.File
import Smos.Actions.Utils
import Smos.Directory.Archive
import Smos.Directory.Resolution
import Smos.Directory.ShouldPrint
import Smos.Report.OptParse
import Smos.Types

allPlainReportOngoingActions :: [Action]
allPlainReportOngoingActions =
  [ reportOngoing,
    prevOngoing,
    nextOngoing,
    firstOngoing,
    lastOngoing,
    enterOngoingFile,
    selectOngoingReport,
    selectOngoingFilter,
    removeOngoingFilter,
    deleteOngoingFilter
  ]

allReportOngoingUsingActions :: [ActionUsing Char]
allReportOngoingUsingActions =
  [ insertOngoingFilter,
    appendOngoingFilter
  ]

reportOngoing :: Action
reportOngoing =
  Action
    { actionName = "reportOngoing",
      actionFunc = modifyEditorCursorS $ \ec -> do
        saveCurrentSmosFile
        now <- gets smosStateNow
        zone <- gets smosStateTimeZone
        dc <- asks $ reportSettingDirectorySettings . configReportSettings
        narc <- liftIO $ produceOngoingReportCursor zone now Nothing HideArchive DontPrint dc
        pure $
          ec
            { editorCursorSelection = ReportSelected,
              editorCursorReportCursor = Just $ ReportOngoing narc
            },
      actionDescription = "Ongoing report"
    }

prevOngoing :: Action
prevOngoing =
  Action
    { actionName = "prevOngoing",
      actionFunc = modifyOngoingReportCursorM ongoingReportCursorPrev,
      actionDescription = "Select the previous entry in the ongoing report"
    }

nextOngoing :: Action
nextOngoing =
  Action
    { actionName = "nextOngoing",
      actionFunc = modifyOngoingReportCursorM ongoingReportCursorNext,
      actionDescription = "Select the next entry in the ongoing report"
    }

firstOngoing :: Action
firstOngoing =
  Action
    { actionName = "firstOngoing",
      actionFunc = modifyOngoingReportCursor ongoingReportCursorFirst,
      actionDescription = "Select the first entry in the ongoing report"
    }

lastOngoing :: Action
lastOngoing =
  Action
    { actionName = "lastOngoing",
      actionFunc = modifyOngoingReportCursor ongoingReportCursorLast,
      actionDescription = "Select the last entry in the ongoing report"
    }

enterOngoingFile :: Action
enterOngoingFile =
  Action
    { actionName = "enterOngoingFile",
      actionFunc = do
        ss <- get
        case editorCursorReportCursor $ smosStateCursor ss of
          Just rc -> case rc of
            ReportOngoing wrc -> do
              dc <- asks $ reportSettingDirectorySettings . configReportSettings
              wd <- liftIO $ resolveDirWorkflowDir dc
              case ongoingReportCursorBuildSmosFileCursor wd wrc of
                Nothing -> pure ()
                Just (fp, sfc) -> void $ switchToCursor fp (Just sfc)
            _ -> pure ()
          Nothing -> pure (),
      actionDescription = "Enter the currently selected ongoing entry"
    }

insertOngoingFilter :: ActionUsing Char
insertOngoingFilter =
  ActionUsing
    { actionUsingName = "insertOngoingFilter",
      actionUsingDescription = "Insert a character into the filter bar",
      actionUsingFunc = \a -> modifyOngoingReportCursorM $ ongoingReportCursorInsert a
    }

appendOngoingFilter :: ActionUsing Char
appendOngoingFilter =
  ActionUsing
    { actionUsingName = "appendOngoingFilter",
      actionUsingDescription = "Append a character onto the filter bar",
      actionUsingFunc = \a -> modifyOngoingReportCursorM $ ongoingReportCursorAppend a
    }

removeOngoingFilter :: Action
removeOngoingFilter =
  Action
    { actionName = "removeOngoingFilter",
      actionDescription = "Remove the character in filter bar before cursor",
      actionFunc = modifyOngoingReportCursorM ongoingReportCursorRemove
    }

deleteOngoingFilter :: Action
deleteOngoingFilter =
  Action
    { actionName = "deleteOngoingFilter",
      actionDescription = "Remove the character in filter bar under cursor",
      actionFunc = modifyOngoingReportCursorM ongoingReportCursorDelete
    }

selectOngoingReport :: Action
selectOngoingReport =
  Action
    { actionName = "selectOngoingReport",
      actionDescription = "Select the ongoing report",
      actionFunc = modifyOngoingReportCursorM ongoingReportCursorSelectReport
    }

selectOngoingFilter :: Action
selectOngoingFilter =
  Action
    { actionName = "selectOngoingFilter",
      actionDescription = "Select the ongoing filter bar",
      actionFunc = modifyOngoingReportCursorM ongoingReportCursorSelectFilter
    }
