{-# LANGUAGE RecordWildCards #-}

module Smos
    ( smos
    , smosWithoutRuntimeConfig
    , startSmosOn
    , module Smos.Config
    ) where

import Import

import Data.Time

import Control.Concurrent
import Control.Concurrent.Async
import System.Exit

import Brick.BChan as Brick
import Brick.Main as Brick
import Graphics.Vty as Vty (defaultConfig, mkVty)

import Smos.Data

import Smos.Actions.File
import Smos.App
import Smos.Config
import Smos.OptParse
import Smos.OptParse.Bare
import Smos.Types

smos :: SmosConfig -> IO ()
smos sc = do
    Instructions p sc' <- getInstructions sc
    startSmosOn p sc'

smosWithoutRuntimeConfig :: SmosConfig -> IO ()
smosWithoutRuntimeConfig sc = do
    p <- getPathArgument
    startSmosOn p sc

startSmosOn :: Path Abs File -> SmosConfig -> IO ()
startSmosOn p sc@SmosConfig {..} = do
    errOrSF <- readSmosFile p
    startF <-
        case errOrSF of
            Nothing -> pure Nothing
            Just (Left err) ->
                die $
                unlines
                    [ "Failed to read smos file"
                    , fromAbsFile p
                    , "could not parse it:"
                    , show err
                    ]
            Just (Right sf) -> pure $ Just sf
    lock <- lockFile p
    case lock of
        Nothing -> die "Failed to lock. Has this file already been opened in another instance of smos?"
        Just fl -> do
            zt <- getZonedTime
            let s = initState zt p fl startF
            chan <- Brick.newBChan maxBound
            Left s' <-
                race
                    (Brick.customMain
                         (Vty.mkVty Vty.defaultConfig)
                         (Just chan)
                         (mkSmosApp sc)
                         s)
                    (eventPusher chan)
            forM_ (smosStateAsyncs s') wait
            saveSmosFile
                (rebuildEditorCursor $ smosStateCursor s')
                (smosStateStartSmosFile s')
                (smosStateFilePath s')
            unlockFile $ smosStateFileLock s'

eventPusher :: BChan SmosEvent -> IO ()
eventPusher chan =
    concurrently_
        (loopEvery 1 (writeBChan chan SmosUpdateTime))
        (loopEvery 5 (writeBChan chan SmosSaveFile))
  where
    loopEvery :: Int -> IO () -> IO ()
    loopEvery i func = do
        func
        threadDelay $ i * 1000 * 1000
        loopEvery i func
