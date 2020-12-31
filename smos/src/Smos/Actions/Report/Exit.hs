{-# LANGUAGE OverloadedStrings #-}

module Smos.Actions.Report.Exit where

import Smos.Actions.Browser
import Smos.Actions.File
import Smos.Types

allPlainReportExitActions :: [Action]
allPlainReportExitActions =
  [ exitReport
  ]

allReportExitUsingActions :: [ActionUsing Char]
allReportExitUsingActions = []

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

-- MkSmosM $ NextT $ pure Stop
