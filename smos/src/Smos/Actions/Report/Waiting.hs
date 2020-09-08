{-# LANGUAGE OverloadedStrings #-}

module Smos.Actions.Report.Waiting where

import Smos.Actions.File
import Smos.Actions.Utils
import Smos.Report.Config
import Smos.Types

allPlainReportWaitingActions :: [Action]
allPlainReportWaitingActions = [reportWaiting]

allReportWaitingUsingActions :: [ActionUsing Char]
allReportWaitingUsingActions = []

reportWaiting :: Action
reportWaiting =
  Action
    { actionName = "reportWaiting",
      actionFunc = modifyEditorCursorS $ \ec -> do
        saveCurrentSmosFile
        dc <- asks $ smosReportConfigDirectoryConfig . configReportConfig
        narc <- liftIO $ produceWaitingReportCursor dc
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
      actionFunc = undefined,
      actionDescription = "TODO"
    }

nextWaiting :: Action
nextWaiting =
  Action
    { actionName = "nextWaiting",
      actionFunc = undefined,
      actionDescription = "TODO"
    }

firstWaiting :: Action
firstWaiting =
  Action
    { actionName = "firstWaiting",
      actionFunc = undefined,
      actionDescription = "TODO"
    }

lastWaiting :: Action
lastWaiting =
  Action
    { actionName = "lastWaiting",
      actionFunc = undefined,
      actionDescription = "TODO"
    }
