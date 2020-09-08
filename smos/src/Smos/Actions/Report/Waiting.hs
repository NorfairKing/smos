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
