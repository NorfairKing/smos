{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Smos.Actions.Convenience
  ( allConveniencePlainActions,
    convDoneAndWaitForResponse,
    convRepinged,
    convRespondedButStillWaiting,
    convNewEntryAndClockIn,
    convArchiveFile,
  )
where

import Control.Category ((>>>))
import Control.Monad.Catch
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Lens.Micro
import Path
import Smos.Actions.Browser
import Smos.Actions.Entry
import Smos.Actions.File
import Smos.Actions.Forest
import Smos.Actions.Utils
import Smos.Archive
import Smos.Cursor.SmosFileEditor
import Smos.Data
import Smos.Report.Config
import Smos.Types

allConveniencePlainActions :: [Action]
allConveniencePlainActions =
  [ convDoneAndWaitForResponse,
    convRepinged,
    convRespondedButStillWaiting,
    convNewEntryAndClockIn,
    convArchiveFile
  ]

convDoneAndWaitForResponse :: Action
convDoneAndWaitForResponse =
  Action
    { actionName = "convDoneAndWaitForResponse",
      actionFunc = modifyFileCursorS $ \sfc -> do
        now <- liftIO getCurrentTime
        let f1 = smosFileCursorSelectedEntryL . entryCursorStateHistoryCursorL %~ stateHistoryCursorSetTodoState now "DONE"
            f2 = smosFileCursorInsertEntryAfterAndSelectHeader
            f3 = smosFileCursorSelectedEntryL . entryCursorHeaderCursorL %~ (\hc -> fromMaybe hc $ headerCursorAppendString "for a response from " hc)
            f4 = smosFileCursorSelectedEntryL . entryCursorStateHistoryCursorL %~ stateHistoryCursorSetTodoState now "WAITING"
            f5 = smosFileCursorSelectedEntryL %~ entryCursorSelectHeaderAtEnd
        pure $ (f1 >>> f2 >>> f3 >>> f4 >>> f5) sfc,
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
        let f1 = smosFileCursorSelectedEntryL . entryCursorStateHistoryCursorL %~ stateHistoryCursorSetTodoState now "DONE"
            f2 = smosFileCursorInsertEntryAfterAndSelectHeader
            f3 = smosFileCursorSelectedEntryL . entryCursorHeaderCursorL %~ (\hc -> fromMaybe hc $ headerCursorAppendString "Ping again" hc)
            f4 = smosFileCursorSelectedEntryL . entryCursorStateHistoryCursorL %~ stateHistoryCursorSetTodoState now "DONE"
            f5 = smosFileCursorInsertEntryAfterAndSelectHeader
            e' =
              e
                { entryStateHistory =
                    StateHistory
                      [ StateHistoryEntry
                          { stateHistoryEntryNewState = entryState e,
                            stateHistoryEntryTimestamp = now
                          }
                      ]
                }
            f6 = smosFileCursorSelectedEntryL .~ makeEntryCursor e'
            f7 = smosFileCursorSelectedEntryL %~ entryCursorSelectWhole
        pure $ (f1 >>> f2 >>> f3 >>> f4 >>> f5 >>> f6 >>> f7) sfc,
      actionDescription =
        "Mark the current task as 'done', add a new entry called 'Ping again' and add a new WAITING entry below that, that duplicates the original entry."
    }

convRespondedButStillWaiting :: Action
convRespondedButStillWaiting =
  Action
    { actionName = "convRespondedButStillWaiting",
      actionFunc = modifyFileCursorS $ \sfc -> do
        let e = rebuildEntryCursor $ sfc ^. smosFileCursorSelectedEntryL
        now <- liftIO getCurrentTime
        let f1 = smosFileCursorSelectedEntryL . entryCursorStateHistoryCursorL %~ stateHistoryCursorSetTodoState now "DONE"
            f2 = smosFileCursorInsertEntryAfterAndSelectHeader
            e' =
              e
                { entryStateHistory =
                    StateHistory
                      [ StateHistoryEntry
                          { stateHistoryEntryNewState = entryState e,
                            stateHistoryEntryTimestamp = now
                          }
                      ]
                }
            f3 = smosFileCursorSelectedEntryL .~ makeEntryCursor e'
            f4 = smosFileCursorSelectedEntryL %~ entryCursorSelectWhole
        pure $ (f1 >>> f2 >>> f3 >>> f4) sfc,
      actionDescription =
        "Mark the current task as 'done' and add a new entry below that duplicates the original entry."
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
            let runArchiveM = liftIO . flip runReaderT dc
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
