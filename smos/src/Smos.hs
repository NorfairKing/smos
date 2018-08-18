{-# LANGUAGE RecordWildCards #-}

module Smos
    ( smos
    , module Smos.Config
    ) where

import Import

import System.Exit

import qualified Data.List.NonEmpty as NE

import Brick.Main as B

import Smos.Cursor.SmosFile
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
    let s = initState p $ fromMaybe emptySmosFile startF
    s' <- defaultMain (mkSmosApp sc) s
    let sf' = rebuildEntireSmosFileCursor $ smosStateCursor s'
    when (startF /= Just sf') $ writeSmosFile p sf'

initState :: Path Abs File -> SmosFile -> SmosState
initState p sf =
    SmosState
        { smosStateFilePath = p
        , smosStateCursor = makeEntireSmosFileCursor sf
        , smosStateKeyHistory = Empty
        , smosStateDebugInfo = DebugInfo {debugInfoLastMatches = Nothing}
        , smosStateShowHelp = False
        , smosStateShowDebug = False
        }

makeEntireSmosFileCursor :: SmosFile -> Maybe SmosFileCursor
makeEntireSmosFileCursor =
    fmap makeSmosFileCursor . NE.nonEmpty . smosFileForest

rebuildEntireSmosFileCursor :: Maybe SmosFileCursor -> SmosFile
rebuildEntireSmosFileCursor =
    SmosFile . maybe [] NE.toList . fmap rebuildSmosFileCursor
