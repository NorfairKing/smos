{-# LANGUAGE RecordWildCards #-}

module Smos
  ( smos,
    smosWithoutRuntimeConfig,
    startSmosOn,
    startSmosWithVtyBuilderOn,
    module Smos.Report.OptParse,
    module Smos.Config,
  )
where

import Brick.BChan as Brick
import Brick.Main as Brick
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.Trans.Resource (withInternalState)
import Data.Maybe
import Graphics.Vty as Vty (Vty, defaultConfig, setWindowTitle)
import Graphics.Vty.CrossPlatform (mkVty)
import Smos.Actions.File
import Smos.App
import Smos.Config
import Smos.Cursor.SmosFileEditor
import Smos.Directory.Resolution
import Smos.OptParse
import Smos.OptParse.Bare
import Smos.Report.OptParse
import Smos.Types
import UnliftIO.Resource

smos :: SmosConfig -> IO ()
smos sc = do
  Instructions mst sc' <- getInstructions sc
  startSmosOn mst sc'

smosWithoutRuntimeConfig :: SmosConfig -> IO ()
smosWithoutRuntimeConfig sc = do
  mst <- getPathArgument
  startSmosOn mst sc

startSmosOn :: Maybe StartingPath -> SmosConfig -> IO ()
startSmosOn mst = startSmosWithVtyBuilderOn vtyBuilder mst
  where
    vtyBuilder = do
      vty <- mkVty defaultConfig
      setWindowTitle vty "smos"
      pure vty

startSmosWithVtyBuilderOn :: IO Vty.Vty -> Maybe StartingPath -> SmosConfig -> IO ()
startSmosWithVtyBuilderOn vtyBuilder mst sc@SmosConfig {..} = runResourceT $ do
  st <- liftIO $ case mst of
    Nothing -> StartingDir <$> resolveDirWorkflowDir (reportSettingDirectorySettings configReportSettings)
    Just st -> pure st
  s <- buildInitialState st
  withInternalState $ \res -> do
    chan <- Brick.newBChan maxBound
    initialVty <- vtyBuilder
    workflowDir <- resolveDirWorkflowDir (reportSettingDirectorySettings configReportSettings)

    Left s' <-
      race
        (Brick.customMain initialVty vtyBuilder (Just chan) (mkSmosApp res workflowDir sc) s)
        (eventPusher chan)
    finalWait $ smosStateAsyncs s'
    case editorCursorFileCursor $ smosStateCursor s' of
      Nothing -> pure ()
      Just sfec -> void $ smosFileEditorCursorSave sfec

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
