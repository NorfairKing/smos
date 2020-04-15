{-# LANGUAGE RecordWildCards #-}

module Smos
  ( smos,
    smosWithoutRuntimeConfig,
    startSmosOn,
    module Smos.Config,
  )
where

import Brick.BChan as Brick
import Brick.Main as Brick
import Control.Concurrent
import Control.Concurrent.Async
import Data.Time
import Graphics.Vty as Vty (defaultConfig, mkVty)
import Import
import Smos.Actions.File
import Smos.App
import Smos.Config
import Smos.Data
import Smos.OptParse
import Smos.OptParse.Bare
import Smos.Types
import System.Exit

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
        die $ unlines ["Failed to read smos file", fromAbsFile p, "could not parse it:", err]
      Just (Right sf) -> pure $ Just sf
  lock <- lockFile p
  case lock of
    Nothing -> die "Failed to lock. Has this file already been opened in another instance of smos?"
    Just fl -> do
      zt <- getZonedTime
      let s = initState zt p fl startF
      chan <- Brick.newBChan maxBound
      let vtyBuilder = mkVty defaultConfig
      initialVty <- vtyBuilder
      Left s' <-
        race
          (Brick.customMain initialVty vtyBuilder (Just chan) (mkSmosApp sc) s)
          (eventPusher chan)
      finalWait $ smosStateAsyncs s'
      saveSmosFile
        (rebuildEditorCursor $ smosStateCursor s')
        (smosStateStartSmosFile s')
        (smosStateFilePath s')
      unlockFile $ smosStateFileLock s'

finalWait :: [Async ()] -> IO ()
finalWait as = do
  as' <-
    fmap catMaybes <$> forM as $ \a -> do
      mEither <- poll a
      pure $
        case mEither of
          Nothing -> Just a
          Just _ -> Nothing
  unless (null as') $ do
    putStrLn $ unwords ["Waiting for", show (length as'), "asynchronous operations to finish"]
    mapM_ wait as'

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
