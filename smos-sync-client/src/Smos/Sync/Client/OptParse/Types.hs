{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Sync.Client.OptParse.Types where

import Control.Monad.Logger
import Data.Validity
import Data.Yaml as Yaml
import GHC.Generics (Generic)
import Path
import Servant.Client (BaseUrl)
import Smos.API
import qualified Smos.Report.OptParse.Types as Report
import Text.Read
import YamlParse.Applicative

data Arguments
  = Arguments Command (Report.FlagsWithConfigFile Flags)
  deriving (Show, Eq)

data Instructions
  = Instructions Dispatch Settings

data Command
  = CommandRegister RegisterFlags
  | CommandLogin LoginFlags
  | CommandSync SyncFlags
  deriving (Show, Eq)

data RegisterFlags
  = RegisterFlags
  deriving (Show, Eq)

data LoginFlags
  = LoginFlags
  deriving (Show, Eq)

data SyncFlags
  = SyncFlags
      { syncFlagContentsDir :: Maybe FilePath,
        syncFlagUUIDFile :: Maybe FilePath,
        syncFlagMetadataDB :: Maybe FilePath,
        syncFlagIgnoreFiles :: Maybe IgnoreFiles
      }
  deriving (Show, Eq)

data Flags
  = Flags
      { flagDirectoryFlags :: Report.DirectoryFlags,
        flagLogLevel :: Maybe LogLevel,
        flagServerUrl :: Maybe String,
        flagUsername :: Maybe Username,
        flagPassword :: Maybe Password,
        flagSessionPath :: Maybe FilePath
      }
  deriving (Show, Eq, Generic)

data Environment
  = Environment
      { envDirectoryEnvironment :: Report.DirectoryEnvironment,
        envLogLevel :: Maybe LogLevel,
        envServerUrl :: Maybe String,
        envContentsDir :: Maybe FilePath,
        envUUIDFile :: Maybe FilePath,
        envMetadataDB :: Maybe FilePath,
        envIgnoreFiles :: Maybe IgnoreFiles,
        envUsername :: Maybe Username,
        envPassword :: Maybe Password,
        envSessionPath :: Maybe FilePath
      }
  deriving (Show, Eq, Generic)

data Configuration
  = Configuration
      { confDirectoryConf :: Report.DirectoryConfiguration,
        confSyncConf :: Maybe SyncConfiguration
      }
  deriving (Show, Eq, Generic)

instance FromJSON Configuration where
  parseJSON = viaYamlSchema

instance YamlSchema Configuration where
  yamlSchema =
    Configuration <$> yamlSchema <*> objectParser "Configuration" (optionalField "sync" "Synchronisation configuration")

data SyncConfiguration
  = SyncConfiguration
      { syncConfLogLevel :: Maybe LogLevel,
        syncConfServerUrl :: Maybe String,
        syncConfContentsDir :: Maybe FilePath,
        syncConfUUIDFile :: Maybe FilePath,
        syncConfMetadataDB :: Maybe FilePath,
        syncConfIgnoreFiles :: Maybe IgnoreFiles,
        syncConfUsername :: Maybe Username,
        syncConfPassword :: Maybe Password,
        syncConfSessionPath :: Maybe FilePath
      }
  deriving (Show, Eq, Generic)

instance FromJSON SyncConfiguration where
  parseJSON = viaYamlSchema

instance YamlSchema SyncConfiguration where
  yamlSchema =
    objectParser "SyncConfiguration" $
      SyncConfiguration
        <$> optionalFieldWith "log-level" "The minimal severity for log messages" (maybeParser parseLogLevel yamlSchema)
        <*> optionalField "server-url" "The url of the sync server. Example: api.smos.cs-syd.eu"
        <*> optionalField "contents-dir" "The directory of the files to synchronise. By default this will be the workflow directory."
        <*> optionalField "uuid-file" "The file in which to store the server uuid"
        <*> optionalField "metadata-db" "The file to store the metadata database in"
        <*> optionalField "ignore-files" "Which files to ignore"
        <*> optionalField "username" "The username to log into the sync server"
        <*> optionalField "password" "The password to log into the sync server. Note that putting the password in a config file in plaintext is not safe. Only use this for automation."
        <*> optionalField "session-path" "The file in which to store the login session cookie"

data Dispatch
  = DispatchRegister RegisterSettings
  | DispatchLogin LoginSettings
  | DispatchSync SyncSettings
  deriving (Show, Eq, Generic)

data SyncSettings
  = SyncSettings
      { syncSetContentsDir :: Path Abs Dir,
        syncSetUUIDFile :: Path Abs File,
        syncSetMetadataDB :: Path Abs File,
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

instance FromJSON IgnoreFiles where
  parseJSON = viaYamlSchema

instance YamlSchema IgnoreFiles where
  yamlSchema =
    alternatives
      [ IgnoreNothing <$ literalString "nothing" <?> "Don't ignore any files",
        IgnoreNothing <$ literalString "hidden" <?> "Ignore hidden files"
      ]

parseLogLevel :: String -> Maybe LogLevel
parseLogLevel s = readMaybe $ "Level" <> s

renderLogLevel :: LogLevel -> String
renderLogLevel = drop 5 . show

data Settings
  = Settings
      { setServerUrl :: BaseUrl,
        setLogLevel :: LogLevel,
        setUsername :: Maybe Username,
        setPassword :: Maybe Password,
        setSessionPath :: Path Abs File
      }
  deriving (Show, Eq, Generic)
