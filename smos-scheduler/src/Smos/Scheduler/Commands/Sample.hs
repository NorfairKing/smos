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
import Smos.Scheduler.OptParse
import Smos.Scheduler.Render
import Smos.Scheduler.Utils
import System.Exit

sample :: Settings -> Path Abs File -> IO ()
sample Settings {..} templateFile = do
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
        let ctx = RenderContext {renderContextTime = now}
        case runRender ctx $ renderTemplate template of
          Left errs -> die $ unlines $ "Error while rendering template: " : map show (NE.toList errs)
          Right rendered -> SB.putStr $ smosFileBS rendered
