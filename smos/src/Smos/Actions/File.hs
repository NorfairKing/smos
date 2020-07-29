{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Actions.File
  ( saveFile,
    saveCurrentSmosFile,
    saveSmosFile,
    switchToFile,
    switchToCursor,
    lockFile,
    unlockFile,
    FL.FileLock,
  )
where

import Data.Maybe
import Data.Time
import Path
import Path.IO
import Smos.Cursor.SmosFile
import Smos.Cursor.SmosFileEditor
import Smos.Data
import Smos.Types
import qualified System.FileLock as FL

saveFile :: Action
saveFile =
  Action
    { actionName = "saveFile",
      actionFunc = saveCurrentSmosFile,
      actionDescription = "Save the current file"
    }

saveCurrentSmosFile :: SmosM ()
saveCurrentSmosFile = do
  SmosState {..} <- get
  let (path, sf') = rebuildEditorCursor smosStateCursor
  liftIO $ saveSmosFile sf' smosStateStartSmosFile path
  now <- liftIO getCurrentTime
  modify
    ( \ss ->
        ss
          { smosStateStartSmosFile = Just sf',
            smosStateUnsavedChanges = False,
            smosStateLastSaved = now
          }
    )

saveSmosFile :: SmosFile -> Maybe SmosFile -> Path Abs File -> IO ()
saveSmosFile sf' smosStateStartSmosFile smosStateFilePath = do
  e <- doesFileExist smosStateFilePath
  when (e && isNothing smosStateStartSmosFile) $ removeFile smosStateFilePath
  ( case smosStateStartSmosFile of
      Nothing -> unless (sf' == emptySmosFile)
      Just sf'' -> unless (sf'' == sf')
    )
    $ do
      ensureDir $ parent smosStateFilePath
      writeSmosFile smosStateFilePath sf'

switchToFile :: Path Abs File -> SmosM ()
switchToFile path = do
  maybeErrOrSmosFile <- liftIO $ readSmosFile path
  case maybeErrOrSmosFile of
    Nothing -> pure () -- Do nothing if the file doesn't exist
    Just errOrSmosFile -> case errOrSmosFile of
      Left _ -> pure () -- Do nothing if the file is not a smos file
      Right sf -> do
        let msfc = smosFileCursorReadyForStartup <$> makeSmosFileCursorEntirely sf
        void $ switchToCursor path msfc

switchToCursor :: Path Abs File -> Maybe SmosFileCursor -> SmosM (Maybe FL.FileLock)
switchToCursor path msfc = do
  ss <- get
  mfl <-
    liftIO $ do
      unlockFile $ smosStateFileLock ss
      lockFile path
  forM_ mfl $ \fl -> do
    now <- liftIO getCurrentTime
    put $
      ss
        { smosStateStartSmosFile = rebuildSmosFileCursorEntirely <$> msfc,
          smosStateFileLock = fl,
          smosStateCursor =
            editorCursorSwitchToFile
              (smosStateCursor ss)
                { editorCursorFileEditorCursor = makeSmosFileEditorCursorFromCursor path msfc
                },
          smosStateUnsavedChanges = False,
          smosStateLastSaved = now
        }
  pure mfl

lockFile :: Path Abs File -> IO (Maybe FL.FileLock)
lockFile p = do
  ensureDir $ parent p
  FL.tryLockFile (fromAbsFile p) FL.Exclusive

unlockFile :: FL.FileLock -> IO ()
unlockFile = FL.unlockFile
