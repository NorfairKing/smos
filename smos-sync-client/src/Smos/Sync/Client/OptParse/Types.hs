{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Sync.Client.OptParse.Types where

import GHC.Generics (Generic)

import qualified Data.Text as T
import Data.Validity
import Data.Yaml as Yaml

import Control.Monad.Logger
import Path
import Servant.Client (BaseUrl)

import Smos.API

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
    , syncFlagUUIDFile :: Maybe FilePath
    , syncFlagMetadataDB :: Maybe FilePath
    , syncFlagIgnoreFiles :: Maybe IgnoreFiles
    }
  deriving (Show, Eq)

data Flags =
  Flags
    { flagReportFlags :: Report.Flags
    , flagLogLevel :: Maybe LogLevel
    , flagUsername :: Maybe Username
    , flagSessionPath :: Maybe FilePath
    }
  deriving (Show, Eq, Generic)

data Environment =
  Environment
    { envReportEnvironment :: Report.Environment
    , envServerUrl :: Maybe String
    , envContentsDir :: Maybe FilePath
    , envUUIDFile :: Maybe FilePath
    , envMetadataDB :: Maybe FilePath
    , envIgnoreFiles :: Maybe IgnoreFiles
    , envSessionPath :: Maybe FilePath
    , envUsername :: Maybe Username
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
    , syncConfUUIDFile :: Maybe FilePath
    , syncConfMetadataDB :: Maybe FilePath
    , syncConfIgnoreFiles :: Maybe IgnoreFiles
    , syncConfUsername :: Maybe Username
    , syncConfSessionPath :: Maybe FilePath
    }
  deriving (Show, Eq, Generic)

instance FromJSON SyncConfiguration where
  parseJSON =
    withObject "SyncConfiguration" $ \o ->
      SyncConfiguration <$> o .:? "server-url" <*> o .:? "contents-dir" <*> o .:? "uuid-file" <*>
      o .:? "metadata-db" <*>
      o .:? "ignore-files" <*>
      o .:? "username" <*>
      o .:? "session-path"

newtype Dispatch =
  DispatchSync SyncSettings
  deriving (Show, Eq, Generic)

data SyncSettings =
  SyncSettings
    { syncSetServerUrl :: BaseUrl
    , syncSetContentsDir :: Path Abs Dir
    , syncSetUUIDFile :: Path Abs File
    , syncSetMetadataDB :: Path Abs File
    , syncSetIgnoreFiles :: IgnoreFiles
    }
  deriving (Show, Eq, Generic)

data IgnoreFiles
  = IgnoreNothing
  | IgnoreHiddenFiles
  deriving (Show, Eq, Generic)

instance Validity IgnoreFiles

instance FromJSON IgnoreFiles where
  parseJSON =
    withText "IgnoreFiles" $ \t ->
      case t of
        "nothing" -> pure IgnoreNothing
        "no" -> pure IgnoreNothing
        "hidden" -> pure IgnoreHiddenFiles
        _ -> fail $ "Unknown 'IgnoreFiles' value: " <> T.unpack t

data Settings =
  Settings
    { setLogLevel :: LogLevel
    , setUsername :: Maybe Username
    , setSessionPath :: Path Abs File
    }
  deriving (Show, Eq, Generic)
