{-# LANGUAGE RecordWildCards #-}

module Smos.Scheduler.Commands.Sample (sample) where

import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.ByteString as SB
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Path
import Smos.CLI.Logging
import Smos.Data
import Smos.Directory.Resolution
import Smos.Scheduler.OptParse
import Smos.Scheduler.Render
import Smos.Scheduler.Schedule
import System.Exit

sample :: Settings -> Path Abs File -> Maybe DestinationPathTemplate -> IO ()
sample Settings {..} templateFile mdpt = runFilteredLogger setLogLevel $ do
  mErrOrTemplate <- liftIO $ readScheduleTemplate templateFile
  case mErrOrTemplate of
    Nothing -> liftIO $ die $ unwords ["Template file not found:", fromAbsFile templateFile]
    Just errOrTemplate -> case errOrTemplate of
      Left err ->
        liftIO $
          die $
            unlines
              [ unwords
                  [ "unparseable template:",
                    fromAbsFile templateFile
                  ],
                err
              ]
      Right template -> do
        let renderTemplateAndPath :: Render (SmosFile, Maybe (Path Rel File))
            renderTemplateAndPath = (,) <$> renderTemplate template <*> traverse renderDestinationPathTemplate mdpt
        errOrRendered <- liftIO $ runRenderNow renderTemplateAndPath
        case errOrRendered of
          Left errs -> liftIO $ die $ unlines $ "Error while rendering template: " : map show (NE.toList errs)
          Right (rendered, mDestinationPath) -> case mDestinationPath of
            Nothing -> liftIO $ SB.putStr $ smosFileBS rendered
            Just destinationRelativePath -> do
              workflowDir <- liftIO $ resolveDirWorkflowDir setDirectorySettings
              let destinationPath = workflowDir </> destinationRelativePath
              liftIO $ writeSmosFile destinationPath rendered
              logInfoN $ T.pack $ unwords ["Rendered template written to:", fromAbsFile destinationPath]
