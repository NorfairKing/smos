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
  s <- buildInitState p
  chan <- Brick.newBChan maxBound
  let vtyBuilder = mkVty defaultConfig
  initialVty <- vtyBuilder
  Left s' <-
    race
      (Brick.customMain initialVty vtyBuilder (Just chan) (mkSmosApp sc) s)
      (eventPusher chan)
  finalWait $ smosStateAsyncs s'
  case editorCursorSum $ smosStateCursor s' of
    EditorCursorFileSelected sfec -> do
      sfec' <- smosFileEditorCursorSave sfec
      smosFileEditorCursorClose sfec'

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
