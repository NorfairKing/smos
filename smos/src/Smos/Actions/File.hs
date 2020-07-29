{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Actions.File
  ( saveFile,
    saveCurrentSmosFile,
    closeCurrentFile,
    switchToFile,
    switchToCursor,
  )
where

import Data.Maybe
import Data.Time
import Path
import Path.IO
import Smos.Actions.Utils
import Smos.Cursor.SmosFile
import Smos.Cursor.SmosFileEditor
import Smos.Data
import Smos.Types

saveFile :: Action
saveFile =
  Action
    { actionName = "saveFile",
      actionFunc = saveCurrentSmosFile,
      actionDescription = "Save the current file"
    }

saveCurrentSmosFile :: SmosM ()
saveCurrentSmosFile = modifySmosFileEditorCursorS $ liftIO . smosFileEditorCursorSave

-- TODO don't change if it's the same file
switchToFile :: Path Abs File -> SmosM ()
switchToFile path = do
  modifyEditorCursorSumS $ \sfec -> do
    mErrOrSmec <- startSmosFileEditorCursor path
    case mErrOrSmec of
      Nothing -> pure sfec -- Couldn't get a lock, do nothing
      Just errOrSmec ->
        case errOrSmec of
          Left _ -> pure sfec -- Do nothing if the file is not a smos file
          Right smec -> do
            saveCurrentSmosFile
            closeCurrentFile
            pure $ EditorCursorFileSelected smec

-- TODO don't change if it's the same file
switchToCursor :: Path Abs File -> Maybe SmosFileCursor -> SmosM ()
switchToCursor path msfc = modifyEditorCursorSumS $ \sfec -> do
  mSmec <- startSmosFileEditorCursorWithCursor path msfc
  case mSmec of
    Nothing -> pure sfec -- Couldn't get a lock, do nothing
    Just smec -> do
      saveCurrentSmosFile
      closeCurrentFile
      pure $ EditorCursorFileSelected smec

-- Note that this leaves the file cursor invalidated, so it must not end up in the editor cursor sum after this.
closeCurrentFile :: SmosM ()
closeCurrentFile = modifySmosFileEditorCursorS $ \sfec -> do
  liftIO $ smosFileEditorCursorClose sfec
  pure sfec
