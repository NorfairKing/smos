{-# LANGUAGE OverloadedStrings #-}

module Smos.Actions.Report.Stuck where

import Data.Time.Zones
import Path
import Smos.Actions.File
import Smos.Actions.Utils
import Smos.Directory.Resolution
import Smos.Directory.ShouldPrint
import Smos.Report.OptParse.Types
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
        dc <- asks $ reportSettingDirectorySettings . configReportSettings
        zone <- liftIO loadLocalTZ
        narc <- liftIO $ produceStuckReportCursor zone DontPrint dc
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
              dc <- asks $ reportSettingDirectorySettings . configReportSettings
              pd <- liftIO $ resolveDirProjectsDir dc
              case stuckReportCursorSelectedFile src of
                Nothing -> pure ()
                Just rp -> void $ switchToFile $ pd </> rp
            _ -> pure ()
          Nothing -> pure (),
      actionDescription = "Enter the currently selected stuck project"
    }
