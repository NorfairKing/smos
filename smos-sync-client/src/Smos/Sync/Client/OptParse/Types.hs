{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Sync.Client.OptParse.Types where

import GHC.Generics (Generic)

import Data.Yaml as Yaml

import Path

import Servant.Client (BaseUrl)

import Smos.Report.Config as Report
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
    }
  deriving (Show, Eq)

newtype Flags =
  Flags
    { flagReportFlags :: Report.Flags
    }
  deriving (Show, Eq, Generic)

data Environment =
  Environment
    { envReportEnvironment :: Report.Environment
    , envServerUrl :: Maybe String
    , envContentsDir :: Maybe FilePath
    , envMetadataFile :: Maybe FilePath
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
    flip (withObject "Configuration") v $ \o ->
      Configuration <$> parseJSON v <*> o .:? "sync"

data SyncConfiguration =
  SyncConfiguration
    { syncConfServerUrl :: Maybe String
    , syncConfContentsDir :: Maybe FilePath
    , syncConfMetadataFile :: Maybe FilePath
    }
  deriving (Show, Eq, Generic)

instance FromJSON SyncConfiguration where
  parseJSON =
    withObject "SyncConfiguration" $ \o ->
      SyncConfiguration <$> o .: "server-url" <*> o .: "contents-dir" <*>
      o .: "metadata-file"

newtype Dispatch =
  DispatchSync SyncSettings
  deriving (Show, Eq, Generic)

data SyncSettings =
  SyncSettings
    { syncSetServerUrl :: BaseUrl
    , syncSetContentsDir :: Path Abs Dir
    , syncSetMetadataFile :: Path Abs File
    }
  deriving (Show, Eq, Generic)

newtype Settings =
  Settings
    { setReportConfig :: SmosReportConfig
    }
  deriving (Show, Eq, Generic)
