{-# LANGUAGE OverloadedStrings #-}

module Smos.Actions.Browser where

import Cursor.Simple.DirForest
import Path
import Smos.Actions.File
import Smos.Actions.Utils
import Smos.Cursor.FileBrowser
import Smos.Report.Config
import Smos.Types

allPlainBrowserActions :: [Action]
allPlainBrowserActions =
  [ selectBrowser,
    browserSelectPrev,
    browserSelectNext,
    browserToggleCollapse,
    browserToggleCollapseRecursively,
    browserEnter
  ]

allBrowserUsingCharActions :: [ActionUsing Char]
allBrowserUsingCharActions = []

browserSelectPrev :: Action
browserSelectPrev =
  Action
    { actionName = "browserSelectPrev",
      actionFunc = modifyFileBrowserCursorM fileBrowserCursorSelectPrev,
      actionDescription = "Select the previous file or directory in the file browser."
    }

browserSelectNext :: Action
browserSelectNext =
  Action
    { actionName = "browserSelectNext",
      actionFunc = modifyFileBrowserCursorM fileBrowserCursorSelectNext,
      actionDescription = "Select the next file or directory in the file browser."
    }

browserToggleCollapse :: Action
browserToggleCollapse =
  Action
    { actionName = "browserToggleCollapse",
      actionFunc = modifyFileBrowserCursorM fileBrowserCursorToggle,
      actionDescription = "Select toggle collapsing the currently selected directory"
    }

browserToggleCollapseRecursively :: Action
browserToggleCollapseRecursively =
  Action
    { actionName = "browserToggleCollapseRecursively",
      actionFunc = modifyFileBrowserCursorM fileBrowserCursorToggleRecursively,
      actionDescription = "Select toggle collapsing the currently selected directory recursively"
    }

browserEnter :: Action
browserEnter =
  Action
    { actionName = "browserEnter",
      actionFunc = do
        ss <- get
        let ec = smosStateCursor ss
        case editorCursorSelection ec of
          BrowserSelected ->
            case editorCursorBrowserCursor ec of
              Nothing -> pure ()
              Just dfc -> case fileBrowserSelected dfc of
                Nothing -> pure ()
                Just (_, FodDir _) -> modifyFileBrowserCursorM fileBrowserCursorToggle
                Just (rd, FodFile rf ()) -> do
                  saveCurrentSmosFile
                  src <- asks configReportConfig
                  wd <- liftIO $ resolveReportWorkflowDir src
                  let path = wd </> rd </> rf
                  switchToFile path
          _ -> pure (),
      actionDescription = "Enter the file if a file is selected, toggle collapsing the directory if a directory is selected"
    }

selectBrowser :: Action
selectBrowser =
  Action
    { actionName = "selectBrowser",
      actionFunc = modifyEditorCursorS $ \ec -> do
        src <- asks configReportConfig
        wd <- liftIO $ resolveReportWorkflowDir src
        dfc' <- startFileBrowserCursor wd
        -- We don't want to move the cursor if the directory hasn't changed.
        -- TODO: We could get rid of this extra checking if the filebrowser had a way of re-syncing while it was going.
        let dfc = case editorCursorBrowserCursor ec of
              Nothing -> dfc'
              Just dfc'' ->
                if rebuildFileBrowserCursor dfc'' == rebuildFileBrowserCursor dfc'
                  then dfc''
                  else dfc'
        pure
          ec
            { editorCursorBrowserCursor = Just dfc,
              editorCursorSelection = BrowserSelected
            },
      actionDescription = "Save the current file and switch to the file browser."
    }
