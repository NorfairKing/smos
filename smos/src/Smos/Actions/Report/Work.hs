{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Smos.Actions.Report.Work where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Time
import Path
import Smos.Actions.File
import Smos.Actions.Utils
import Smos.Cursor.Report.Work
import Smos.Report.Archive
import Smos.Report.Config
import Smos.Report.Period
import Smos.Report.ShouldPrint
import Smos.Report.Work
import Smos.Types

allPlainReportWorkActions :: [Action]
allPlainReportWorkActions =
  [ reportWork
  ]

allReportWorkUsingActions :: [ActionUsing Char]
allReportWorkUsingActions =
  []

reportWork :: Action
reportWork =
  Action
    { actionName = "reportWork",
      actionFunc = modifyEditorCursorS $ \ec -> do
        saveCurrentSmosFile
        dc <- asks $ smosReportConfigDirectoryConfig . configReportConfig
        now <- liftIO getZonedTime
        -- TODO get these pieces of config from the report config
        let ctx =
              WorkReportContext
                { workReportContextNow = now,
                  workReportContextProjectsSubdir = Just [reldir|projects|],
                  workReportContextBaseFilter = Just defaultWorkBaseFilter,
                  workReportContextCurrentContext = Nothing,
                  workReportContextTimeProperty = Nothing,
                  workReportContextTime = Nothing,
                  workReportContextAdditionalFilter = Nothing,
                  workReportContextContexts = M.empty,
                  workReportContextChecks = S.empty,
                  workReportContextSorter = Nothing,
                  workReportContextWaitingThreshold = 7,
                  workReportContextStuckThreshold = 21
                }

        wrc <- liftIO $ produceWorkReportCursor HideArchive DontPrint dc ctx
        pure $
          ec
            { editorCursorSelection = ReportSelected,
              editorCursorReportCursor = Just $ ReportWork wrc
            },
      actionDescription = "Work report"
    }
