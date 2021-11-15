{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Sync.Client.OptParse.Types where

import Autodocodec
import Control.Monad.Logger
import Data.Validity
import GHC.Generics (Generic)
import Path
import Servant.Client (BaseUrl)
import Smos.API
import qualified Smos.Report.OptParse.Types as Report
import Text.Read

data Arguments
  = Arguments Command (Report.FlagsWithConfigFile Flags)
  deriving (Show)

data Instructions
  = Instructions Dispatch Settings

data Command
  = CommandRegister RegisterFlags
  | CommandLogin LoginFlags
  | CommandSync SyncFlags
  deriving (Show)

data RegisterFlags
  = RegisterFlags
  deriving (Show)

data LoginFlags
  = LoginFlags
  deriving (Show)

data SyncFlags = SyncFlags
  { syncFlagContentsDir :: Maybe FilePath,
    syncFlagUUIDFile :: Maybe FilePath,
    syncFlagMetadataDB :: Maybe FilePath,
    syncFlagIgnoreFiles :: Maybe IgnoreFiles,
    syncFlagBackupDir :: Maybe FilePath
  }
  deriving (Show)

data Flags = Flags
  { flagDirectoryFlags :: Report.DirectoryFlags,
    flagLogLevel :: Maybe LogLevel,
    flagServerUrl :: Maybe String,
    flagUsername :: Maybe Username,
    flagPassword :: Maybe Password,
    flagDataDir :: Maybe FilePath,
    flagCacheDir :: Maybe FilePath,
    flagSessionPath :: Maybe FilePath
  }
  deriving (Show, Generic)

data Environment = Environment
  { envDirectoryEnvironment :: Report.DirectoryEnvironment,
    envLogLevel :: Maybe LogLevel,
    envServerUrl :: Maybe String,
    envContentsDir :: Maybe FilePath,
    envUUIDFile :: Maybe FilePath,
    envMetadataDB :: Maybe FilePath,
    envIgnoreFiles :: Maybe IgnoreFiles,
    envUsername :: Maybe Username,
    envPassword :: Maybe Password,
    envSessionPath :: Maybe FilePath,
    envBackupDir :: Maybe FilePath
  }
  deriving (Show, Generic)

data Configuration = Configuration
  { confDirectoryConf :: Report.DirectoryConfiguration,
    confSyncConf :: Maybe SyncConfiguration
  }
  deriving (Show, Generic)

instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> Report.directoryConfigurationObjectCodec .= confDirectoryConf
        <*> optionalField "sync" "Synchronisation configuration" .= confSyncConf

data SyncConfiguration = SyncConfiguration
  { syncConfLogLevel :: Maybe LogLevel,
    syncConfServerUrl :: Maybe String,
    syncConfDataDir :: Maybe FilePath,
    syncConfCacheDir :: Maybe FilePath,
    syncConfContentsDir :: Maybe FilePath,
    syncConfUUIDFile :: Maybe FilePath,
    syncConfMetadataDB :: Maybe FilePath,
    syncConfIgnoreFiles :: Maybe IgnoreFiles,
    syncConfUsername :: Maybe Username,
    syncConfPassword :: Maybe Password,
    syncConfSessionPath :: Maybe FilePath,
    syncConfBackupDir :: Maybe FilePath
  }
  deriving (Show, Generic)

instance HasCodec SyncConfiguration where
  codec =
    object "SyncConfiguration" $
      SyncConfiguration
        <$> optionalFieldWith
          "log-level"
          (bimapCodec parseLogLevel renderLogLevel codec)
          "The minimal severity for log messages"
          .= syncConfLogLevel
        <*> optionalField
          "server-url"
          "The url of the sync server. Example: api.smos.online"
          .= syncConfServerUrl
        <*> optionalField
          "data-dir"
          "The directory to store state metadata in (not the contents to be synced)"
          .= syncConfDataDir
        <*> optionalField
          "cache-dir"
          "The directory to cache state data in"
          .= syncConfCacheDir
        <*> optionalField
          "contents-dir"
          "The directory of the files to synchronise. By default this will be the workflow directory."
          .= syncConfContentsDir
        <*> optionalField
          "uuid-file"
          "The file in which to store the server uuid"
          .= syncConfUUIDFile
        <*> optionalField
          "metadata-db"
          "The file to store the metadata database in"
          .= syncConfMetadataDB
        <*> optionalField
          "ignore-files"
          "Which files to ignore"
          .= syncConfIgnoreFiles
        <*> optionalField
          "username"
          "The username to log into the sync server"
          .= syncConfUsername
        <*> optionalField
          "password"
          "The password to log into the sync server. Note that putting the password in a config file in plaintext is not safe. Only use this for automation."
          .= syncConfPassword
        <*> optionalField
          "session-path"
          "The file in which to store the login session cookie"
          .= syncConfSessionPath
        <*> optionalField
          "backup-dir"
          "The directory to store backups in when a sync conflict happens"
          .= syncConfBackupDir

data Dispatch
  = DispatchRegister RegisterSettings
  | DispatchLogin LoginSettings
  | DispatchSync SyncSettings
  deriving (Show, Eq, Generic)

data SyncSettings = SyncSettings
  { syncSetContentsDir :: Path Abs Dir,
    syncSetUUIDFile :: Path Abs File,
    syncSetMetadataDB :: Path Abs File,
    syncSetBackupDir :: Path Abs Dir,
    syncSetIgnoreFiles :: IgnoreFiles
  }
  deriving (Show, Eq, Generic)

data RegisterSettings
  = RegisterSettings
  deriving (Show, Eq, Generic)

data LoginSettings
  = LoginSettings
  deriving (Show, Eq, Generic)

data IgnoreFiles
  = IgnoreNothing
  | IgnoreHiddenFiles
  deriving (Show, Eq, Generic)

instance Validity IgnoreFiles

instance HasCodec IgnoreFiles where
  codec =
    stringConstCodec
      [ (IgnoreNothing, "nothing"),
        (IgnoreHiddenFiles, "hidden")
      ]
      <??> [ "nothing: Don't ignore any files",
             "hidden: Ignore hidden files"
           ]

parseLogLevel :: String -> Either String LogLevel
parseLogLevel s = case readMaybe $ "Level" <> s of
  Nothing -> Left $ unwords ["Unknown log level: " <> show s]
  Just ll -> Right ll

renderLogLevel :: LogLevel -> String
renderLogLevel = drop 5 . show

data Settings = Settings
  { setServerUrl :: BaseUrl,
    setLogLevel :: LogLevel,
    setUsername :: Maybe Username,
    setPassword :: Maybe Password,
    setSessionPath :: Path Abs File
  }
  deriving (Show, Generic)
