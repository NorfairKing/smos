{-# LANGUAGE OverloadedStrings #-}

module Smos.Actions.Report where

import Smos.Actions.Browser
import Smos.Actions.File
import Smos.Actions.Report.Next
import Smos.Actions.Report.Ongoing
import Smos.Actions.Report.Stuck
import Smos.Actions.Report.Timestamps
import Smos.Actions.Report.Waiting
import Smos.Actions.Report.Work
import Smos.Types

allPlainReportActions :: [Action]
allPlainReportActions =
  concat
    [ allPlainReportExitActions,
      allPlainReportNextActions,
      allPlainReportWaitingActions,
      allPlainReportOngoingActions,
      allPlainReportTimestampsActions,
      allPlainReportStuckActions,
      allPlainReportWorkActions
    ]

allPlainReportExitActions :: [Action]
allPlainReportExitActions =
  [ exitReport
  ]

allReportUsingActions :: [ActionUsing Char]
allReportUsingActions =
  concat
    [ allReportNextActionsUsingActions,
      allReportWaitingUsingActions,
      allReportOngoingUsingActions,
      allReportTimestampsUsingActions,
      allReportStuckUsingActions,
      allReportWorkUsingActions
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
