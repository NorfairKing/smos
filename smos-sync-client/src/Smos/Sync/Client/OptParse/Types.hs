{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Sync.Client.OptParse.Types where

import GHC.Generics (Generic)

import qualified Data.Text as T
import Data.Validity
import Data.Yaml as Yaml
import Text.Read

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

data Command
  = CommandRegister RegisterFlags
  | CommandLogin LoginFlags
  | CommandSync SyncFlags
  deriving (Show, Eq)

data RegisterFlags =
  RegisterFlags
  deriving (Show, Eq)

data LoginFlags =
  LoginFlags
  deriving (Show, Eq)

data SyncFlags =
  SyncFlags
    { syncFlagContentsDir :: Maybe FilePath
    , syncFlagUUIDFile :: Maybe FilePath
    , syncFlagMetadataDB :: Maybe FilePath
    , syncFlagIgnoreFiles :: Maybe IgnoreFiles
    }
  deriving (Show, Eq)

data Flags =
  Flags
    { flagReportFlags :: Report.Flags
    , flagLogLevel :: Maybe LogLevel
    , flagServerUrl :: Maybe String
    , flagUsername :: Maybe Username
    , flagPassword :: Maybe Password
    , flagSessionPath :: Maybe FilePath
    }
  deriving (Show, Eq, Generic)

data Environment =
  Environment
    { envReportEnvironment :: Report.Environment
    , envLogLevel :: Maybe LogLevel
    , envServerUrl :: Maybe String
    , envContentsDir :: Maybe FilePath
    , envUUIDFile :: Maybe FilePath
    , envMetadataDB :: Maybe FilePath
    , envIgnoreFiles :: Maybe IgnoreFiles
    , envUsername :: Maybe Username
    , envPassword :: Maybe Password
    , envSessionPath :: Maybe FilePath
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
    { syncConfLogLevel :: Maybe LogLevel
    , syncConfServerUrl :: Maybe String
    , syncConfContentsDir :: Maybe FilePath
    , syncConfUUIDFile :: Maybe FilePath
    , syncConfMetadataDB :: Maybe FilePath
    , syncConfIgnoreFiles :: Maybe IgnoreFiles
    , syncConfUsername :: Maybe Username
    , syncConfPassword :: Maybe Password
    , syncConfSessionPath :: Maybe FilePath
    }
  deriving (Show, Eq, Generic)

instance FromJSON SyncConfiguration where
  parseJSON =
    withObject "SyncConfiguration" $ \o ->
      SyncConfiguration <$>
      (do ms <- o .:? "log-level"
          case ms of
            Nothing -> pure Nothing
            Just s ->
              case parseLogLevel s of
                Nothing -> fail $ "Unknown log level: " <> s
                Just ll -> pure $ Just ll) <*>
      o .:? "server-url" <*>
      o .:? "contents-dir" <*>
      o .:? "uuid-file" <*>
      o .:? "metadata-db" <*>
      o .:? "ignore-files" <*>
      o .:? "username" <*>
      o .:? "password" <*>
      o .:? "session-path"

data Dispatch
  = DispatchRegister RegisterSettings
  | DispatchLogin LoginSettings
  | DispatchSync SyncSettings
  deriving (Show, Eq, Generic)

data SyncSettings =
  SyncSettings
    { syncSetContentsDir :: Path Abs Dir
    , syncSetUUIDFile :: Path Abs File
    , syncSetMetadataDB :: Path Abs File
    , syncSetIgnoreFiles :: IgnoreFiles
    }
  deriving (Show, Eq, Generic)

data RegisterSettings =
  RegisterSettings
  deriving (Show, Eq, Generic)

data LoginSettings =
  LoginSettings
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

parseLogLevel :: String -> Maybe LogLevel
parseLogLevel s = readMaybe $ "Level" <> s

renderLogLevel :: LogLevel -> String
renderLogLevel = drop 5 . show

data Settings =
  Settings
    { setServerUrl :: BaseUrl
    , setLogLevel :: LogLevel
    , setUsername :: Maybe Username
    , setPassword :: Maybe Password
    , setSessionPath :: Path Abs File
    }
  deriving (Show, Eq, Generic)
