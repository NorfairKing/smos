{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Smos.Actions.File
    ( saveFile
    , saveCurrentSmosFile
    , saveSmosFile
    , switchToFile
    , lockFile
    , unlockFile
    , FL.FileLock
    ) where

import Path
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
    modify (\ss -> ss {smosStateStartSmosFile = Just sf'})

saveSmosFile :: SmosFile -> Maybe SmosFile -> Path Abs File -> IO ()
saveSmosFile sf' smosStateStartSmosFile smosStateFilePath = do
    (case smosStateStartSmosFile of
         Nothing -> unless (sf' == emptySmosFile)
         Just sf'' -> unless (sf'' == sf')) $
        writeSmosFile smosStateFilePath sf'

switchToFile :: Path Abs File -> SmosFileCursor -> SmosM (Maybe FL.FileLock)
switchToFile path sfc = do
    ss <- get
    mfl <-
        liftIO $ do
            unlockFile $ smosStateFileLock ss
            lockFile path
    forM_ mfl $ \fl ->
        put $
        ss
        { smosStateStartSmosFile = Just (rebuildSmosFileCursorEntirely sfc)
        , smosStateFilePath = path
        , smosStateFileLock = fl
        , smosStateCursor =
              editorCursorSwitchToFile
                  (smosStateCursor ss) {editorCursorFileCursor = Just sfc}
        }
    pure mfl

lockFile :: Path Abs File -> IO (Maybe FL.FileLock)
lockFile p = FL.tryLockFile (fromAbsFile p) FL.Exclusive

unlockFile :: FL.FileLock -> IO ()
unlockFile = FL.unlockFile
