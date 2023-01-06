{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Smos.Actions.Convenience
  ( allConveniencePlainActions,
    convDoneAndWaitForResponse,
    convRepinged,
    convResponded,
    convRespondedButStillWaiting,
    convNewEntryAndClockIn,
    convArchiveFile,
    convOpenUrl,
    convCopyContentsToClipboard,
    convUrlWaitingForReview,
  )
where

import Control.Monad.Catch
import Control.Monad.Logger
import qualified Data.ByteString as SB
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Lens.Micro
import Path
import Path.IO
import Smos.Actions.Browser
import Smos.Actions.Entry
import Smos.Actions.File
import Smos.Actions.Forest
import Smos.Actions.Utils
import Smos.Archive.Commands.File
import Smos.Archive.OptParse.Types as Archive
import Smos.Cursor.SmosFileEditor
import Smos.Data
import Smos.Report.Config
import Smos.Types
import System.Exit
import System.Process

allConveniencePlainActions :: [Action]
allConveniencePlainActions =
  [ convDoneAndWaitForResponse,
    convRepinged,
    convResponded,
    convRespondedButStillWaiting,
    convNewEntryAndClockIn,
    convArchiveFile,
    convOpenUrl,
    convCopyContentsToClipboard,
    convUrlWaitingForReview
  ]

convDoneAndWaitForResponse :: Action
convDoneAndWaitForResponse =
  Action
    { actionName = "convDoneAndWaitForResponse",
      actionFunc = modifyFileCursorS $ \sfc -> do
        now <- liftIO getCurrentTime
        let steps :: [SmosFileCursor -> SmosFileCursor]
            steps =
              [ smosFileCursorSelectedEntryL . entryCursorStateHistoryCursorL %~ stateHistoryCursorSetTodoState now "DONE",
                smosFileCursorInsertEntryAfterAndSelectHeader,
                smosFileCursorSelectedEntryL . entryCursorHeaderCursorL %~ (\hc -> fromMaybe hc $ headerCursorAppendString "for a response from " hc),
                smosFileCursorSelectedEntryL . entryCursorStateHistoryCursorL %~ stateHistoryCursorSetTodoState now "WAITING",
                smosFileCursorSelectedEntryL %~ entryCursorSelectHeaderAtEnd
              ]
        pure $ applySteps steps sfc,
      actionDescription =
        "Mark the current task as 'Done', add a new entry called 'Waiting for a response from ' WAITINg entry with the header selected at the end."
    }

convRepinged :: Action
convRepinged =
  Action
    { actionName = "convRepinged",
      actionFunc = modifyFileCursorS $ \sfc -> do
        let e = rebuildEntryCursor $ sfc ^. smosFileCursorSelectedEntryL
        now <- liftIO getCurrentTime
        let steps :: [SmosFileCursor -> SmosFileCursor]
            steps =
              [ smosFileCursorSelectedEntryL . entryCursorStateHistoryCursorL %~ stateHistoryCursorSetTodoState now "DONE",
                smosFileCursorInsertEntryAfterAndSelectHeader,
                smosFileCursorSelectedEntryL . entryCursorHeaderCursorL %~ (\hc -> fromMaybe hc $ headerCursorAppendString "Ping" hc),
                smosFileCursorSelectedEntryL . entryCursorStateHistoryCursorL %~ stateHistoryCursorSetTodoState now "DONE",
                smosFileCursorInsertEntryAfterAndSelectHeader,
                let e' =
                      e
                        { entryStateHistory =
                            StateHistory
                              [ StateHistoryEntry
                                  { stateHistoryEntryNewState = entryState e,
                                    stateHistoryEntryTimestamp = now
                                  }
                              ],
                          entryLogbook = emptyLogbook
                        }
                 in smosFileCursorSelectedEntryL .~ makeEntryCursor e',
                smosFileCursorSelectedEntryL %~ entryCursorSelectWhole
              ]
        pure $ applySteps steps sfc,
      actionDescription =
        "Mark the current task as 'done', add a new entry called 'Ping' and add a new WAITING entry below that, that duplicates the original entry."
    }

convResponded :: Action
convResponded =
  Action
    { actionName = "convResponded",
      actionFunc = modifyFileCursorS $ \sfc -> do
        now <- liftIO getCurrentTime
        let steps :: [SmosFileCursor -> SmosFileCursor]
            steps =
              [ smosFileCursorSelectedEntryL . entryCursorStateHistoryCursorL %~ stateHistoryCursorSetTodoState now "DONE",
                smosFileCursorInsertEntryAfterAndSelectHeader,
                smosFileCursorSelectedEntryL . entryCursorHeaderCursorL %~ (\hc -> fromMaybe hc $ headerCursorAppendString "Respond" hc),
                smosFileCursorSelectedEntryL . entryCursorStateHistoryCursorL %~ stateHistoryCursorSetTodoState now "DONE",
                smosFileCursorSelectedEntryL %~ entryCursorSelectWhole
              ]
        pure $ applySteps steps sfc,
      actionDescription =
        "Mark the current task as 'done' and add a done entry with 'Responded' below the original entry."
    }

convRespondedButStillWaiting :: Action
convRespondedButStillWaiting =
  Action
    { actionName = "convRespondedButStillWaiting",
      actionFunc = modifyFileCursorS $ \sfc -> do
        let e = rebuildEntryCursor $ sfc ^. smosFileCursorSelectedEntryL
        now <- liftIO getCurrentTime
        let steps :: [SmosFileCursor -> SmosFileCursor]
            steps =
              [ smosFileCursorSelectedEntryL . entryCursorStateHistoryCursorL %~ stateHistoryCursorSetTodoState now "DONE",
                smosFileCursorInsertEntryAfterAndSelectHeader,
                smosFileCursorSelectedEntryL . entryCursorHeaderCursorL %~ (\hc -> fromMaybe hc $ headerCursorAppendString "Respond" hc),
                smosFileCursorSelectedEntryL . entryCursorStateHistoryCursorL %~ stateHistoryCursorSetTodoState now "DONE",
                smosFileCursorInsertEntryAfterAndSelectHeader,
                let e' =
                      e
                        { entryStateHistory =
                            StateHistory
                              [ StateHistoryEntry
                                  { stateHistoryEntryNewState = entryState e,
                                    stateHistoryEntryTimestamp = now
                                  }
                              ],
                          entryLogbook = emptyLogbook
                        }
                 in smosFileCursorSelectedEntryL .~ makeEntryCursor e',
                smosFileCursorSelectedEntryL %~ entryCursorSelectWhole
              ]
        pure $ applySteps steps sfc,
      actionDescription =
        "Mark the current task as 'done', add a done entry with 'Responded' below the orginal entry, and add a new entry below that duplicates the original entry."
    }

convNewEntryAndClockIn :: Action
convNewEntryAndClockIn =
  Action
    { actionName = "convNewEntryAndClockIn",
      actionFunc = do
        actionFunc forestInsertEntryAfterAndSelectHeader
        actionFunc entrySelectWhole
        actionFunc forestClockOutEverywhereInAllFilesAndClockInHere
        actionFunc entrySelectHeaderAtEnd,
      actionDescription = "Create a new entry and clock in immediately"
    }

convArchiveFile :: Action
convArchiveFile =
  Action
    { actionName = "convArchiveFile",
      actionFunc = do
        saveCurrentSmosFile
        closeCurrentFile
        modifyMSmosFileEditorCursorMS $ \case
          Nothing -> pure Nothing
          Just sfec -> do
            dc <- asks $ smosReportConfigDirectoryConfig . configReportConfig
            let runArchiveM =
                  liftIO
                    . flip
                      runReaderT
                      ( Archive.Settings
                          { Archive.setDirectorySettings = dc,
                            Archive.setLogLevel = LevelError
                          }
                      )
            let sourceFile = smosFileEditorPath sfec
            mdf <- runArchiveM $ (Just <$> determineToFile sourceFile) `catch` (\(_ :: NotInWorkflowDir) -> pure Nothing)
            case mdf of
              Nothing -> do
                addErrorMessage "The file that you are trying to archive is not in the workflow directory."
                pure (Just sfec)
              Just df -> do
                result <- moveToArchive sourceFile df (rebuildSmosFileEditorCursor sfec)
                case result of
                  MoveDestinationAlreadyExists df' -> do
                    addErrorMessage $ "The destination file already exists: " <> T.pack (fromAbsFile df')
                    pure (Just sfec)
                  ArchivedSuccesfully -> pure Nothing
        actionFunc selectBrowserProjects,
      actionDescription = "Archive the current file and switch to the file browser in the projects directory. Note that this action cannot be undone. It will not archive a file outside of the workflow directory but still switch to the projects directory in the browser."
    }

convOpenUrl :: Action
convOpenUrl =
  Action
    { actionName = "convOpenUrl",
      actionFunc = requireUnsandboxed $
        modifyEntryCursorS $ \ec -> do
          let e = rebuildEntryCursor ec
          let murl = do
                urlPv <- M.lookup "url" $ entryProperties e
                pure $ propertyValueText urlPv

          forM_ murl $ \url -> do
            mXdgOpenExecutable <- findExecutable [relfile|xdg-open|]
            case mXdgOpenExecutable of
              Nothing -> addErrorMessage "No xdg-open executable found."
              Just xdgOpenExecutable -> do
                runSmosAsync $ do
                  let cp =
                        (proc (fromAbsFile xdgOpenExecutable) [T.unpack url])
                          { std_in = CreatePipe,
                            std_out = CreatePipe,
                            std_err = CreatePipe
                          }
                  _ <- createProcess cp
                  pure ()
          pure ec,
      actionDescription = "Open the url in the 'url' property of the currently selected entry"
    }

convCopyContentsToClipboard :: Action
convCopyContentsToClipboard = do
  Action
    { actionName = "convCopyContentsToClipboard",
      actionFunc = requireUnsandboxed $
        modifyEntryCursorS $ \ec -> do
          let e = rebuildEntryCursor ec
          forM_ (entryContents e) $ \cts -> do
            mXclipExecutable <- findExecutable [relfile|xclip|]
            case mXclipExecutable of
              Nothing -> addErrorMessage "No xclip executable found."
              Just xclipExecutable -> do
                exitCode <- liftIO $
                  withSystemTempDir "smos-clipboard" $ \tdir -> do
                    tfile <- resolveFile tdir "contents-file"
                    SB.writeFile (fromAbsFile tfile) (TE.encodeUtf8 (contentsText cts))
                    rawSystem (fromAbsFile xclipExecutable) ["-in", "-selection", "clipboard", fromAbsFile tfile]
                case exitCode of
                  ExitFailure c -> addErrorMessage $ T.pack $ unwords ["xclip failed with exit code:", show c]
                  ExitSuccess -> pure ()
          pure ec,
      actionDescription = "Copy the contents of the selected entry to the system clipboard"
    }

convUrlWaitingForReview :: Action
convUrlWaitingForReview =
  Action
    { actionName = "convUrlWaitingForReview",
      actionFunc = modifyFileCursorS $ \sfc -> do
        now <- liftIO getCurrentTime
        let steps :: [SmosFileCursor -> SmosFileCursor]
            steps =
              [ smosFileCursorSelectedEntryL . entryCursorStateHistoryCursorL %~ stateHistoryCursorSetTodoState now "DONE",
                smosFileCursorInsertEntryAfterAndSelectHeader,
                smosFileCursorSelectedEntryL . entryCursorHeaderCursorL %~ (\hc -> fromMaybe hc $ headerCursorAppendString "for review here" hc),
                smosFileCursorSelectedEntryL %~ entryCursorSelectWhole,
                smosFileCursorSelectedEntryL . entryCursorStateHistoryCursorL %~ stateHistoryCursorSetTodoState now "WAITING",
                smosFileCursorSelectedEntryL . entryCursorPropertiesCursorL %~ (Just . propertiesCursorAddOrSelect "url"),
                smosFileCursorSelectedEntryL %~ entryCursorSelectProperties
              ]
        pure $ applySteps steps sfc,
      actionDescription = "Mark the currently selected entry as DONE, add a new one below with state WAITING for review, and select the url property."
    }

applySteps :: [a -> a] -> (a -> a)
applySteps = \case
  [] -> id
  (f : fs) -> \a -> applySteps fs (f a)
