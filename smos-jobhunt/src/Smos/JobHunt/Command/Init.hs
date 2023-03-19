{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.JobHunt.Command.Init
  ( smosJobHuntInit,
    initApplicationProject,
    initSmosFilePath,
    initSmosFile,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Path
import Path.IO
import Smos.Data
import Smos.JobHunt.OptParse.Types
import System.Exit

smosJobHuntInit :: (MonadIO m, MonadLogger m) => Settings -> InitSettings -> m ()
smosJobHuntInit Settings {..} initSettings = void $ initApplicationProject setJobHuntDirectory setGoal initSettings

initApplicationProject :: (MonadIO m, MonadLogger m) => Path Abs Dir -> Maybe PropertyValue -> InitSettings -> m (Path Abs File)
initApplicationProject pd mGoal InitSettings {..} = do
  now <- liftIO getCurrentTime
  case (,) <$> initSmosFilePath initSettingCompany <*> initSmosFile now initSettingCompany initSettingContactEmail initSettingUrl mGoal of
    Nothing -> liftIO $ die "Failed to produce a valid smos file."
    Just (relFile, smosFile) -> do
      let projectFile = pd </> relFile
      exists <- doesFileExist projectFile
      if exists
        then liftIO $ die $ "ERROR: not overriding existing smos file: " <> fromAbsFile projectFile
        else do
          logInfoN $ T.pack $ unwords ["Writing job application project to", fromAbsFile projectFile]
          liftIO $ writeSmosFile projectFile smosFile
          pure projectFile

initSmosFilePath :: Text -> Maybe (Path Rel File)
initSmosFilePath companyName =
  parseRelFile $
    mconcat
      [ T.unpack (T.replace " " "-" (T.toLower companyName)),
        ".smos"
      ]

initSmosFile :: UTCTime -> Text -> Maybe Text -> Maybe Text -> Maybe PropertyValue -> Maybe SmosFile
initSmosFile now companyName mEmailAddress mUrl mGoalProperty = do
  titleEntry'' <- newEntry <$> header (companyName <> " Application")
  titleEntry' <- entrySetState now (Just "TODO") titleEntry''
  goalProperty <- case mGoalProperty of
    Nothing -> propertyValue $ "Apply at " <> companyName
    Just gp -> pure gp
  mEmailAddressProperty <- mapM propertyValue mEmailAddress
  mUrlProperty <- mapM propertyValue mUrl
  let titleEntry =
        titleEntry'
          { entryProperties =
              M.fromList $
                concat
                  [ [("goal", goalProperty)],
                    [("email_address", emailAddressProperty) | emailAddressProperty <- maybeToList mEmailAddressProperty],
                    [("url", urlProperty) | urlProperty <- maybeToList mUrlProperty]
                  ]
          }
  applyEntry' <- newEntry <$> header "Apply"
  applyEntry <- entrySetState now (Just "DONE") applyEntry'
  waitingEntry'' <- newEntry <$> header ("for a response to my application from " <> companyName)
  waitingEntry' <- entrySetState now (Just "WAITING") waitingEntry''
  waitingThresholdPropertyName <- propertyName "waiting_threshold"
  waitingThresholdProperty <- propertyValue "1 week"
  let waitingEntry = entrySetProperty waitingThresholdPropertyName waitingThresholdProperty waitingEntry'
  pure $
    makeSmosFile
      [ Node
          titleEntry
          [ Node applyEntry [],
            Node waitingEntry []
          ]
      ]
