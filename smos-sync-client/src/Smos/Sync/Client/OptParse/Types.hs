{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Sync.Client.OptParse.Types where

import GHC.Generics (Generic)

import qualified Data.Text as T
import Data.Yaml as Yaml

import Control.Monad.Logger
import Path
import Servant.Client (BaseUrl)

import qualified Smos.Report.OptParse.Types as Report

data Arguments =
  Arguments Command Flags
  deriving (Show, Eq)

data Instructions =
  Instructions Dispatch Settings

newtype Command =
  CommandSync SyncFlags
  deriving (Show, Eq)

data SyncFlags =
  SyncFlags
    { syncFlagServerUrl :: Maybe String
    , syncFlagContentsDir :: Maybe FilePath
    , syncFlagMetadataFile :: Maybe FilePath
    , syncFlagIgnoreFiles :: Maybe IgnoreFiles
    }
  deriving (Show, Eq)

data Flags =
  Flags
    { flagReportFlags :: Report.Flags
    , flagLogLevel :: Maybe LogLevel
    }
  deriving (Show, Eq, Generic)

data Environment =
  Environment
    { envReportEnvironment :: Report.Environment
    , envServerUrl :: Maybe String
    , envContentsDir :: Maybe FilePath
    , envMetadataFile :: Maybe FilePath
    , envIgnoreFiles :: Maybe IgnoreFiles
    }
  deriving (Show, Eq, Generic)

data Configuration =
  Configuration
    { confReportConf :: Report.Configuration
    , confSyncConf :: Maybe SyncConfiguration
    }
  deriving (Show, Eq, Generic)

instance FromJSON Configuration where
  parseJSON v =
    flip (withObject "Configuration") v $ \o -> Configuration <$> parseJSON v <*> o .:? "sync"

data SyncConfiguration =
  SyncConfiguration
    { syncConfServerUrl :: Maybe String
    , syncConfContentsDir :: Maybe FilePath
    , syncConfMetadataFile :: Maybe FilePath
    , syncConfIgnoreFiles :: Maybe IgnoreFiles
    }
  deriving (Show, Eq, Generic)

instance FromJSON SyncConfiguration where
  parseJSON =
    withObject "SyncConfiguration" $ \o ->
      SyncConfiguration <$> o .: "server-url" <*> o .: "contents-dir" <*> o .: "metadata-file" <*>
      o .:? "ignore-files"

newtype Dispatch =
  DispatchSync SyncSettings
  deriving (Show, Eq, Generic)

data SyncSettings =
  SyncSettings
    { syncSetServerUrl :: BaseUrl
    , syncSetContentsDir :: Path Abs Dir
    , syncSetMetadataFile :: Path Abs File
    , syncSetIgnoreFiles :: IgnoreFiles
    }
  deriving (Show, Eq, Generic)

data IgnoreFiles
  = IgnoreNothing
  | IgnoreHiddenFiles
  deriving (Show, Eq, Generic)

instance FromJSON IgnoreFiles where
  parseJSON =
    withText "IgnoreFiles" $ \t ->
      case t of
        "nothing" -> pure IgnoreNothing
        "no" -> pure IgnoreNothing
        "hidden" -> pure IgnoreHiddenFiles
        _ -> fail $ "Unknown 'IgnoreFiles' value: " <> T.unpack t

newtype Settings =
  Settings
    { setLogLevel :: LogLevel
    }
  deriving (Show, Eq, Generic)
