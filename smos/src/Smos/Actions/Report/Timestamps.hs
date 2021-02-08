{-# LANGUAGE OverloadedStrings #-}

module Smos.Actions.Report.Timestamps where

import Data.Time
import Smos.Actions.File
import Smos.Actions.Utils
import Smos.Report.Archive
import Smos.Report.Config
import Smos.Report.Period
import Smos.Report.ShouldPrint
import Smos.Types

allPlainReportTimestampsActions :: [Action]
allPlainReportTimestampsActions =
  [ reportTimestamps,
    prevTimestamps,
    nextTimestamps,
    firstTimestamps,
    lastTimestamps,
    enterTimestampsFile,
    selectTimestampsReport,
    selectTimestampsFilter,
    removeTimestampsFilter,
    deleteTimestampsFilter
  ]

allReportTimestampsUsingActions :: [ActionUsing Char]
allReportTimestampsUsingActions =
  [ insertTimestampsFilter,
    appendTimestampsFilter
  ]

reportTimestamps :: Action
reportTimestamps =
  Action
    { actionName = "reportTimestamps",
      actionFunc = modifyEditorCursorS $ \ec -> do
        saveCurrentSmosFile
        dc <- asks $ smosReportConfigDirectoryConfig . configReportConfig
        now <- liftIO getZonedTime
        narc <- liftIO $ produceTimestampsReportCursor now Today Nothing HideArchive DontPrint dc
        pure $
          ec
            { editorCursorSelection = ReportSelected,
              editorCursorReportCursor = Just $ ReportTimestamps narc
            },
      actionDescription = "Timestamps report"
    }

prevTimestamps :: Action
prevTimestamps =
  Action
    { actionName = "prevTimestamps",
      actionFunc = modifyTimestampsReportCursorM timestampsReportCursorPrev,
      actionDescription = "Select the previous entry in the timestamps report"
    }

nextTimestamps :: Action
nextTimestamps =
  Action
    { actionName = "nextTimestamps",
      actionFunc = modifyTimestampsReportCursorM timestampsReportCursorNext,
      actionDescription = "Select the next entry in the timestamps report"
    }

firstTimestamps :: Action
firstTimestamps =
  Action
    { actionName = "firstTimestamps",
      actionFunc = modifyTimestampsReportCursor timestampsReportCursorFirst,
      actionDescription = "Select the first entry in the timestamps report"
    }

lastTimestamps :: Action
lastTimestamps =
  Action
    { actionName = "lastTimestamps",
      actionFunc = modifyTimestampsReportCursor timestampsReportCursorLast,
      actionDescription = "Select the last entry in the timestamps report"
    }

enterTimestampsFile :: Action
enterTimestampsFile =
  Action
    { actionName = "enterTimestampsFile",
      actionFunc = do
        ss <- get
        case editorCursorReportCursor $ smosStateCursor ss of
          Just rc -> case rc of
            ReportTimestamps wrc -> do
              dc <- asks $ smosReportConfigDirectoryConfig . configReportConfig
              wd <- liftIO $ resolveDirWorkflowDir dc
              case timestampsReportCursorBuildSmosFileCursor wd wrc of
                Nothing -> pure ()
                Just (fp, sfc) -> void $ switchToCursor fp (Just sfc)
            _ -> pure ()
          Nothing -> pure (),
      actionDescription = "Enter the currently selected timestamps entry"
    }

insertTimestampsFilter :: ActionUsing Char
insertTimestampsFilter =
  ActionUsing
    { actionUsingName = "insertTimestampsFilter",
      actionUsingDescription = "Insert a character into the filter bar",
      actionUsingFunc = \a -> modifyTimestampsReportCursorM $ timestampsReportCursorInsert a
    }

appendTimestampsFilter :: ActionUsing Char
appendTimestampsFilter =
  ActionUsing
    { actionUsingName = "appendTimestampsFilter",
      actionUsingDescription = "Append a character onto the filter bar",
      actionUsingFunc = \a -> modifyTimestampsReportCursorM $ timestampsReportCursorAppend a
    }

removeTimestampsFilter :: Action
removeTimestampsFilter =
  Action
    { actionName = "removeTimestampsFilter",
      actionDescription = "Remove the character in filter bar before cursor",
      actionFunc = modifyTimestampsReportCursorM timestampsReportCursorRemove
    }

deleteTimestampsFilter :: Action
deleteTimestampsFilter =
  Action
    { actionName = "deleteTimestampsFilter",
      actionDescription = "Remove the character in filter bar under cursor",
      actionFunc = modifyTimestampsReportCursorM timestampsReportCursorDelete
    }

selectTimestampsReport :: Action
selectTimestampsReport =
  Action
    { actionName = "selectTimestampsReport",
      actionDescription = "Select the timestamps report",
      actionFunc = modifyTimestampsReportCursorM timestampsReportCursorSelectReport
    }

selectTimestampsFilter :: Action
selectTimestampsFilter =
  Action
    { actionName = "selectTimestampsFilter",
      actionDescription = "Select the timestamps filter bar",
      actionFunc = modifyTimestampsReportCursorM timestampsReportCursorSelectFilter
    }
