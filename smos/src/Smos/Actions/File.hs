{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Smos.Actions.File
    ( saveFile
    , saveCurrentSmosFile
    ) where

import Smos.Data

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
    when (smosStateStartSmosFile /= Just sf') $
        liftIO $ writeSmosFile smosStateFilePath sf'
    modify (\ss -> ss {smosStateStartSmosFile = Just sf'})
