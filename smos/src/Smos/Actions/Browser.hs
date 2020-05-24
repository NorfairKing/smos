{-# LANGUAGE OverloadedStrings #-}

module Smos.Actions.Browser where

import Cursor.Simple.DirForest
import Data.DirForest
import Path
import Smos.Actions.Utils
import Smos.Report.Config
import Smos.Types

allPlainBrowserActions :: [Action]
allPlainBrowserActions =
  [ selectBrowser,
    browserSelectPrev,
    browserSelectNext,
    browserToggleCollapse,
    browserToggleCollapseRecursively
  ]

allBrowserUsingCharActions :: [ActionUsing Char]
allBrowserUsingCharActions = []

browserSelectPrev :: Action
browserSelectPrev =
  Action
    { actionName = "browserSelectPrev",
      actionFunc = modifyBrowserCursorM dirForestCursorSelectPrev,
      actionDescription = "Select the previous file or directory in the file browser."
    }

browserSelectNext :: Action
browserSelectNext =
  Action
    { actionName = "browserSelectNext",
      actionFunc = modifyBrowserCursorM dirForestCursorSelectNext,
      actionDescription = "Select the next file or directory in the file browser."
    }

browserToggleCollapse :: Action
browserToggleCollapse =
  Action
    { actionName = "browserToggleCollapse",
      actionFunc = modifyBrowserCursorM dirForestCursorToggle,
      actionDescription = "Select toggle collapsing the currently selected directory"
    }

browserToggleCollapseRecursively :: Action
browserToggleCollapseRecursively =
  Action
    { actionName = "browserToggleCollapseRecursively",
      actionFunc = modifyBrowserCursorM dirForestCursorToggleRecursively,
      actionDescription = "Select toggle collapsing the currently selected directory recursively"
    }

selectBrowser :: Action
selectBrowser =
  Action
    { actionName = "selectBrowser",
      actionFunc = do
        src <- asks configReportConfig
        wd <- liftIO $ resolveReportWorkflowDir src
        ad <- liftIO $ resolveReportArchiveDir src
        let filePred fp = fileExtension fp == ".smos"
            dirPred fp = case stripProperPrefix ad fp of
              Nothing -> True
              Just _ -> False
        df <- readNonHiddenFiltered filePred dirPred wd (\_ -> pure ())
        let dfc = makeDirForestCursor df
        modifyEditorCursor $ \ec ->
          ec
            { editorCursorBrowserCursor = dfc,
              editorCursorSelection = BrowserSelected
            },
      actionDescription = "Save the current file and switch to the file browser."
    }
