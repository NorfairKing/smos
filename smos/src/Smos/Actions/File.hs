{-# LANGUAGE OverloadedStrings #-}

module Smos.Actions.File
  ( saveFile,
    saveCurrentSmosFile,
    closeCurrentFile,
    switchToFile,
    switchToCursor,
  )
where

import Path
import Smos.Actions.Utils
import Smos.Cursor.SmosFileEditor
import Smos.Types

saveFile :: Action
saveFile =
  Action
    { actionName = "saveFile",
      actionFunc = saveCurrentSmosFile,
      actionDescription = "Save the current file"
    }

saveCurrentSmosFile :: SmosM ()
saveCurrentSmosFile = modifyMSmosFileEditorCursorMS $ mapM $ liftIO . smosFileEditorCursorSave

switchToFile :: Path Abs File -> SmosM ()
switchToFile path = modifyEditorCursorS $ \ec -> do
  if (smosFileEditorPath <$> editorCursorFileCursor ec) == Just path
    then pure ec {editorCursorSelection = FileSelected} -- Just don't change anything
    else do
      mErrOrSmec <- startSmosFileEditorCursor path
      case mErrOrSmec of
        Nothing -> pure ec -- Couldn't get a lock, do nothing
        Just errOrSmec ->
          case errOrSmec of
            Left _ -> pure ec -- Do nothing if the file is not a smos file
            Right smec -> do
              saveCurrentSmosFile
              closeCurrentFile
              pure $ ec {editorCursorSelection = FileSelected, editorCursorFileCursor = Just smec}

switchToCursor :: Path Abs File -> Maybe SmosFileCursor -> SmosM ()
switchToCursor path msfc = modifyEditorCursorS $ \ec -> do
  saveCurrentSmosFile
  closeCurrentFile
  mSmec <- startSmosFileEditorCursorWithCursor path msfc
  case mSmec of
    Nothing -> pure ec -- Couldn't get a lock, do nothing
    Just smec -> do
      pure $ ec {editorCursorSelection = FileSelected, editorCursorFileCursor = Just smec}

-- Note that this leaves the file cursor invalidated, so it must not end up in the editor cursor sum after this.
closeCurrentFile :: SmosM ()
closeCurrentFile = modifySmosFileEditorCursorS $ \sfec -> do
  liftIO $ smosFileEditorCursorClose sfec
  pure sfec
