{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Smos.Actions.Browser where

import Cursor.FileOrDir
import Data.Text (Text)
import Path
import Smos.Actions.File
import Smos.Actions.Utils
import Smos.Cursor.FileBrowser
import Smos.Report.Config
import Smos.Types

allPlainBrowserActions :: [Action]
allPlainBrowserActions =
  [ selectBrowserProjects,
    selectBrowserWorkflow,
    selectBrowserArchive,
    selectBrowserReview,
    selectBrowserClient,
    selectBrowserSide,
    browserSelectPrev,
    browserSelectNext,
    browserStartNew,
    browserStartNewBelowAtStart,
    browserStartNewBelowAtEnd,
    browserStopNew,
    browserRemoveChar,
    browserDeleteChar,
    browserSelectPrevChar,
    browserSelectNextChar,
    browserToggleCollapse,
    browserToggleCollapseRecursively,
    browserEnter,
    browserRemoveEmptyDir,
    browserArchive,
    browserCompleteDir,
    browserCompleteFile,
    browserUndo,
    browserUndoAny,
    browserRedo,
    browserRedoAny
  ]

allBrowserUsingCharActions :: [ActionUsing Char]
allBrowserUsingCharActions =
  [ browserInsertChar,
    browserAppendChar
  ]

-- TODO: rename this to browserSelectPrevFileOrDir ?
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

browserStartNew :: Action
browserStartNew =
  Action
    { actionName = "browserStartNew",
      actionFunc = modifyFileBrowserCursorM fileBrowserCursorStartNew,
      actionDescription = "Start a new file or directory in the file browser."
    }

browserStartNewBelowAtStart :: Action
browserStartNewBelowAtStart =
  Action
    { actionName = "browserStartNewBelowAtStart",
      actionFunc = modifyFileBrowserCursorM fileBrowserCursorStartNewBelowAtStart,
      actionDescription = "Start a new file or directory within the selected directory in the file browser, at the start."
    }

browserStartNewBelowAtEnd :: Action
browserStartNewBelowAtEnd =
  Action
    { actionName = "browserStartNewBelowAtEnd",
      actionFunc = modifyFileBrowserCursorM fileBrowserCursorStartNewBelowAtEnd,
      actionDescription = "Start a new file or directory within the selected directory in the file browser, at the end."
    }

browserStopNew :: Action
browserStopNew =
  Action
    { actionName = "browserStopNew",
      actionFunc = modifyFileBrowserCursorM fileBrowserCursorStopNew,
      actionDescription = "Abort creating the new file or directory in the file browser."
    }

browserInsertChar :: ActionUsing Char
browserInsertChar =
  ActionUsing
    { actionUsingName = "browserInsertChar",
      actionUsingFunc = \c -> modifyFileBrowserCursorM $ fileBrowserCursorInsertChar c,
      actionUsingDescription = "Insert a character into the new file or directory being created in the file browser."
    }

browserAppendChar :: ActionUsing Char
browserAppendChar =
  ActionUsing
    { actionUsingName = "browserAppendChar",
      actionUsingFunc = \c -> modifyFileBrowserCursorM $ fileBrowserCursorAppendChar c,
      actionUsingDescription = "Append a character into the new file or directory being created in the file browser."
    }

browserRemoveChar :: Action
browserRemoveChar =
  Action
    { actionName = "browserRemoveChar",
      actionFunc = modifyFileBrowserCursorM fileBrowserCursorRemoveChar,
      actionDescription = "Remove a characer from the new file or directory being created in the file browser."
    }

browserDeleteChar :: Action
browserDeleteChar =
  Action
    { actionName = "browserDeleteChar",
      actionFunc = modifyFileBrowserCursorM fileBrowserCursorDeleteChar,
      actionDescription = "Delete a characer from the new file or directory being created in the file browser."
    }

browserSelectNextChar :: Action
browserSelectNextChar =
  Action
    { actionName = "browserSelectNextChar",
      actionFunc = modifyFileBrowserCursorM fileBrowserCursorSelectNextChar,
      actionDescription = "Select the next characer in the new file or directory being created in the file browser."
    }

browserSelectPrevChar :: Action
browserSelectPrevChar =
  Action
    { actionName = "browserSelectPrevChar",
      actionFunc = modifyFileBrowserCursorM fileBrowserCursorSelectPrevChar,
      actionDescription = "Select the previous characer in the new file or directory being created in the file browser."
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
        case editorCursorBrowserCursor $ smosStateCursor ss of
          Just dfc ->
            case fileBrowserSelected dfc of
              Nothing -> pure ()
              Just (_, _, InProgress _) -> pure ()
              Just (_, _, Existent (FodDir _)) -> modifyFileBrowserCursorM fileBrowserCursorToggle
              Just (base, rd, Existent (FodFile rf ())) -> do
                let path = base </> rd </> rf
                switchToFile path
          Nothing -> pure (),
      actionDescription = "Enter the file if a file is selected, toggle collapsing the directory if a directory is selected"
    }

selectBrowserWorkflow :: Action
selectBrowserWorkflow = selectBrowserHelper "Workflow" resolveReportWorkflowDir

selectBrowserProjects :: Action
selectBrowserProjects = selectBrowserHelper "Projects" resolveReportProjectsDir

selectBrowserArchive :: Action
selectBrowserArchive = selectBrowserHelper "Archive" resolveReportArchiveDir

selectBrowserReview :: Action
selectBrowserReview = selectBrowserHelper "Review" (fmap (</> [reldir|review|]) . resolveReportWorkflowDir)

selectBrowserClient :: Action
selectBrowserClient = selectBrowserHelper "Client" (fmap (</> [reldir|client|]) . resolveReportProjectsDir)

selectBrowserSide :: Action
selectBrowserSide = selectBrowserHelper "Side" (fmap (</> [reldir|side|]) . resolveReportProjectsDir)

selectBrowserHelper :: Text -> (SmosReportConfig -> IO (Path Abs Dir)) -> Action
selectBrowserHelper dirName dirFunc =
  Action
    { actionName = ActionName $ "selectBrowser" <> dirName,
      actionFunc = modifyEditorCursorS $ \ec -> do
        saveCurrentSmosFile
        closeCurrentFile
        src <- asks configReportConfig
        wd <- liftIO $ dirFunc src
        dfc' <- startFileBrowserCursor wd
        -- We don't want to move the cursor if the directory hasn't changed.
        -- We could get rid of this extra checking if the filebrowser had a way of re-syncing while it was going.
        -- but even then we shouldn't because the syncing might not have happned soon enough.
        let dfc = case editorCursorBrowserCursor ec of
              Just dfc'' ->
                if rebuildFileBrowserCursor dfc'' == rebuildFileBrowserCursor dfc'
                  then dfc''
                  else dfc'
              Nothing -> dfc'
        pure $
          ec
            { editorCursorSelection = BrowserSelected,
              editorCursorFileCursor = Nothing,
              editorCursorBrowserCursor = Just dfc
            },
      actionDescription = "Save the current file and switch to the file browser in the " <> dirName <> " directory."
    }

browserRemoveEmptyDir :: Action
browserRemoveEmptyDir =
  Action
    { actionName = "browserRemoveEmptyDir",
      actionFunc = modifyFileBrowserCursorS fileBrowserRmEmptyDir,
      actionDescription = "Remove the currently selected empty directory. This does nothing if the directory is not empty."
    }

browserArchive :: Action
browserArchive =
  Action
    { actionName = "browserArchive",
      actionFunc = modifyFileBrowserCursorS $ \fbc -> do
        src <- asks configReportConfig
        ad <- liftIO $ resolveReportArchiveDir src
        wd <- liftIO $ resolveReportWorkflowDir src
        fileBrowserArchiveFile wd ad fbc,
      actionDescription = "Remove the currently selected empty directory. This does nothing if the directory is not empty."
    }

browserCompleteDir :: Action
browserCompleteDir =
  Action
    { actionName = "browserCompleteDir",
      actionFunc = modifyFileBrowserCursorS fileBrowserCompleteToDir,
      actionDescription = "Complete the current file or directory in progress to a new directory."
    }

browserCompleteFile :: Action
browserCompleteFile =
  Action
    { actionName = "browserCompleteFile",
      actionFunc = do
        modifyFileBrowserCursorS fileBrowserCompleteToFile
        actionFunc browserEnter,
      actionDescription = "Complete the current file or directory in progress to a new file."
    }

browserUndo :: Action
browserUndo =
  Action
    { actionName = "browserUndo",
      actionFunc = modifyFileBrowserCursorSM fileBrowserUndo,
      actionDescription = "Undo the last action non-movement action in the file browser."
    }

browserUndoAny :: Action
browserUndoAny =
  Action
    { actionName = "browserUndoAny",
      actionFunc = modifyFileBrowserCursorSM fileBrowserUndoAny,
      actionDescription = "Undo the last action in the file browser, even if it was a movement."
    }

browserRedo :: Action
browserRedo =
  Action
    { actionName = "browserRedo",
      actionFunc = modifyFileBrowserCursorSM fileBrowserRedo,
      actionDescription = "Redo the last non-movement action in the file browser."
    }

browserRedoAny :: Action
browserRedoAny =
  Action
    { actionName = "browserRedoAny",
      actionFunc = modifyFileBrowserCursorSM fileBrowserRedoAny,
      actionDescription = "Redo the last action in the file browser, even if it was a movement."
    }
