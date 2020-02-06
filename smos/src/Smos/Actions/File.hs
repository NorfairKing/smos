{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Actions.File
  ( saveFile
  , saveCurrentSmosFile
  , saveSmosFile
  , switchToFile
  , lockFile
  , unlockFile
  , FL.FileLock
  ) where

import Data.Maybe

import Data.Time
import Path
import Path.IO
import qualified System.FileLock as FL

import Smos.Data

import Smos.Cursor.SmosFile

import Smos.Types

saveFile :: Action
saveFile =
  Action
    { actionName = "saveFile"
    , actionFunc = saveCurrentSmosFile
    , actionDescription = "Save the current file"
    }

saveCurrentSmosFile :: SmosM ()
saveCurrentSmosFile = do
  SmosState {..} <- get
  let sf' = rebuildEditorCursor smosStateCursor
  liftIO $ saveSmosFile sf' smosStateStartSmosFile smosStateFilePath
  now <- liftIO getCurrentTime
  modify
    (\ss ->
       ss
         { smosStateStartSmosFile = Just sf'
         , smosStateUnsavedChanges = False
         , smosStateLastSaved = now
         })

saveSmosFile :: SmosFile -> Maybe SmosFile -> Path Abs File -> IO ()
saveSmosFile sf' smosStateStartSmosFile smosStateFilePath = do
  e <- doesFileExist smosStateFilePath
  when (e && isNothing smosStateStartSmosFile) $ removeFile smosStateFilePath
  (case smosStateStartSmosFile of
     Nothing -> unless (sf' == emptySmosFile)
     Just sf'' -> unless (sf'' == sf')) $ do
    ensureDir $ parent smosStateFilePath
    writeSmosFile smosStateFilePath sf'

switchToFile :: Path Abs File -> SmosFileCursor -> SmosM (Maybe FL.FileLock)
switchToFile path sfc = do
  ss <- get
  mfl <-
    liftIO $ do
      unlockFile $ smosStateFileLock ss
      lockFile path
  forM_ mfl $ \fl -> do
    now <- liftIO getCurrentTime
    put $
      ss
        { smosStateStartSmosFile = Just (rebuildSmosFileCursorEntirely sfc)
        , smosStateFilePath = path
        , smosStateFileLock = fl
        , smosStateCursor =
            editorCursorSwitchToFile (smosStateCursor ss) {editorCursorFileCursor = Just sfc}
        , smosStateUnsavedChanges = False
        , smosStateLastSaved = now
        }
  pure mfl

lockFile :: Path Abs File -> IO (Maybe FL.FileLock)
lockFile p = do
  ensureDir $ parent p
  FL.tryLockFile (fromAbsFile p) FL.Exclusive

unlockFile :: FL.FileLock -> IO ()
unlockFile = FL.unlockFile
