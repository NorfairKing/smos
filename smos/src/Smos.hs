{-# LANGUAGE RecordWildCards #-}

module Smos
    ( smos
    , module Smos.Config
    ) where

import Import

import System.Exit

import Brick.Main as B

import Smos.Cursor.Editor
import Smos.Data

import Smos.App
import Smos.Config
import Smos.OptParse
import Smos.Types

smos :: SmosConfig -> IO ()
smos sc@SmosConfig {..} = do
    Instructions p Settings <- getInstructions sc
    errOrSF <- readSmosFile p
    startF <-
        case errOrSF of
            Nothing -> pure Nothing
            Just (Left err) -> die $ show err
            Just (Right sf) -> pure $ Just sf
    let s = initState p startF
    s' <- defaultMain (mkSmosApp sc) s
    let sf' = rebuildEditorCursor $ smosStateCursor s'
    when (startF /= Just sf') $ writeSmosFile p sf'
