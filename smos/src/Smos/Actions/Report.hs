{-# LANGUAGE OverloadedStrings #-}

module Smos.Actions.Report where

import Smos.Actions.Browser
import Smos.Actions.File
import Smos.Actions.Report.Next
import Smos.Actions.Report.Waiting
import Smos.Types

allPlainReportExitActions :: [Action]
allPlainReportExitActions =
  [ exitReport
  ]

allPlainReportActions :: [Action]
allPlainReportActions =
  concat
    [ allPlainReportExitActions,
      allPlainReportNextActions,
      allPlainReportWaitingActions
    ]

allReportUsingActions :: [ActionUsing Char]
allReportUsingActions =
  concat
    [ allReportNextActionsUsingActions,
      allReportWaitingUsingActions
    ]

-- Exit a Report
-- If there is a file open, go to it (this already works via another Action?)
-- If there is no file open, go to the browser in the workflow dir
exitReport :: Action
exitReport =
  Action
    { actionName = "exitReport",
      actionDescription = "Exit any smos report, back to open file or browser",
      actionFunc = do
        ec <- gets smosStateCursor
        case editorCursorLastOpenedFile ec of
          Just fp -> switchToFile fp
          Nothing -> actionFunc selectBrowserWorkflow
    }
