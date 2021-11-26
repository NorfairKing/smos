{-# LANGUAGE RecordWildCards #-}

module Smos.Scheduler.Commands.Sample
  ( sample,
  )
where

import qualified Data.ByteString as SB
import qualified Data.List.NonEmpty as NE
import Data.Time
import Path
import Smos.Data
import Smos.Report.Config
import Smos.Scheduler.OptParse
import Smos.Scheduler.Render
import Smos.Scheduler.Utils
import System.Exit

sample :: Settings -> Path Abs File -> Maybe DestinationPathTemplate -> IO ()
sample Settings {..} templateFile mdpt = do
  mErrOrTemplate <- readScheduleTemplate templateFile
  case mErrOrTemplate of
    Nothing -> die $ unwords ["Template file not found:", fromAbsFile templateFile]
    Just errOrTemplate -> case errOrTemplate of
      Left err ->
        die $
          unlines
            [ unwords
                [ "unparseable template:",
                  fromAbsFile templateFile
                ],
              err
            ]
      Right template -> do
        now <- getZonedTime
        let renderTemplateAndPath :: Render (SmosFile, Maybe (Path Rel File))
            renderTemplateAndPath = (,) <$> renderTemplate template <*> traverse renderDestinationPathTemplate mdpt
        let ctx = RenderContext {renderContextTime = now}
        case runRender ctx renderTemplateAndPath of
          Left errs -> die $ unlines $ "Error while rendering template: " : map show (NE.toList errs)
          Right (rendered, mDestinationPath) -> case mDestinationPath of
            Nothing -> SB.putStr $ smosFileBS rendered
            Just destinationRelativePath -> do
              workflowDir <- resolveDirWorkflowDir setDirectorySettings
              let destinationPath = workflowDir </> destinationRelativePath
              writeSmosFile destinationPath rendered
              putStrLn $ unwords ["Rendered template written to:", fromAbsFile destinationPath]
