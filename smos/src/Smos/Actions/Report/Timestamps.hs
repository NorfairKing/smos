{-# LANGUAGE OverloadedStrings #-}

module Smos.Actions.Report.Timestamps where

import Smos.Actions.File
import Smos.Actions.Utils
import Smos.Report.Archive
import Smos.Report.Config
import Smos.Report.ShouldPrint
import Smos.Types

allPlainReportTimestampsActions :: [Action]
allPlainReportTimestampsActions =
  [ reportTimestamps,
    enterTimestampsFile,
    prevTimestamps,
    nextTimestamps,
    firstTimestamps,
    lastTimestamps
  ]

allReportTimestampsUsingActions :: [ActionUsing Char]
allReportTimestampsUsingActions = []

reportTimestamps :: Action
reportTimestamps =
  Action
    { actionName = "reportTimestamps",
      actionFunc = modifyEditorCursorS $ \ec -> do
        saveCurrentSmosFile
        dc <- asks $ smosReportConfigDirectoryConfig . configReportConfig
        narc <- liftIO $ produceTimestampsReportCursor HideArchive DontPrint dc
        pure $
          ec
            { editorCursorSelection = ReportSelected,
              editorCursorReportCursor = Just $ ReportTimestamps narc
            },
      actionDescription = "Timestamps report"
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
