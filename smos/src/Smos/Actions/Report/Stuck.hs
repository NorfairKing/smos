{-# LANGUAGE OverloadedStrings #-}

module Smos.Actions.Report.Stuck where

import Data.Time
import Path
import Smos.Actions.File
import Smos.Actions.Utils
import Smos.Report.Config
import Smos.Report.ShouldPrint
import Smos.Types

allPlainReportStuckActions :: [Action]
allPlainReportStuckActions =
  [ reportStuck,
    prevStuck,
    nextStuck,
    firstStuck,
    lastStuck,
    enterStuckFile
  ]

allReportStuckUsingActions :: [ActionUsing Char]
allReportStuckUsingActions =
  []

reportStuck :: Action
reportStuck =
  Action
    { actionName = "reportStuck",
      actionFunc = modifyEditorCursorS $ \ec -> do
        saveCurrentSmosFile
        dc <- asks $ smosReportConfigDirectoryConfig . configReportConfig
        now <- liftIO getZonedTime
        narc <- liftIO $ produceStuckReportCursor (zonedTimeZone now) DontPrint dc
        pure $
          ec
            { editorCursorSelection = ReportSelected,
              editorCursorReportCursor = Just $ ReportStuck narc
            },
      actionDescription = "Stuck report"
    }

prevStuck :: Action
prevStuck =
  Action
    { actionName = "prevStuck",
      actionFunc = modifyStuckReportCursorM stuckReportCursorPrev,
      actionDescription = "Select the previous entry in the stuck report"
    }

nextStuck :: Action
nextStuck =
  Action
    { actionName = "nextStuck",
      actionFunc = modifyStuckReportCursorM stuckReportCursorNext,
      actionDescription = "Select the next entry in the stuck report"
    }

firstStuck :: Action
firstStuck =
  Action
    { actionName = "firstStuck",
      actionFunc = modifyStuckReportCursor stuckReportCursorFirst,
      actionDescription = "Select the first entry in the stuck report"
    }

lastStuck :: Action
lastStuck =
  Action
    { actionName = "lastStuck",
      actionFunc = modifyStuckReportCursor stuckReportCursorLast,
      actionDescription = "Select the last entry in the stuck report"
    }

enterStuckFile :: Action
enterStuckFile =
  Action
    { actionName = "enterStuckFile",
      actionFunc = do
        ss <- get
        case editorCursorReportCursor $ smosStateCursor ss of
          Just rc -> case rc of
            ReportStuck src -> do
              dc <- asks $ smosReportConfigDirectoryConfig . configReportConfig
              pd <- liftIO $ resolveDirProjectsDir dc
              case stuckReportCursorSelectedFile src of
                Nothing -> pure ()
                Just rp -> void $ switchToFile $ pd </> rp
            _ -> pure ()
          Nothing -> pure (),
      actionDescription = "Enter the currently selected stuck project"
    }
