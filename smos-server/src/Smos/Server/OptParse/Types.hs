{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Server.OptParse.Types where

import Control.Monad.Logger
import Data.Word
import Data.Yaml as Yaml
import GHC.Generics (Generic)
import Looper
import Path
import Smos.API
import Text.Read
import YamlParse.Applicative

data Arguments
  = Arguments Command Flags
  deriving (Show, Eq)

data Instructions
  = Instructions Dispatch Settings

newtype Command
  = CommandServe ServeFlags
  deriving (Show, Eq)

data ServeFlags = ServeFlags
  { serveFlagLogLevel :: !(Maybe LogLevel),
    serveFlagUUIDFile :: !(Maybe FilePath),
    serveFlagDatabaseFile :: !(Maybe FilePath),
    serveFlagSigningKeyFile :: !(Maybe FilePath),
    serveFlagPort :: !(Maybe Int),
    serveFlagMaxBackupsPerUser :: !(Maybe Word),
    serveFlagMaxBackupSizePerUser :: !(Maybe Word64),
    serveFlagAutoBackupLooperFlags :: !LooperFlags,
    serveFlagBackupGarbageCollectionLooperFlags :: !LooperFlags,
    serveFlagAdmin :: !(Maybe Username)
  }
  deriving (Show, Eq)

data Flags = Flags
  { flagConfigFile :: !(Maybe FilePath)
  }
  deriving (Show, Eq, Generic)

data Environment = Environment
  { envConfigFile :: !(Maybe FilePath),
    envLogLevel :: !(Maybe LogLevel),
    envUUIDFile :: !(Maybe FilePath),
    envDatabaseFile :: !(Maybe FilePath),
    envSigningKeyFile :: !(Maybe FilePath),
    envPort :: !(Maybe Int),
    envMaxBackupsPerUser :: !(Maybe Word),
    envMaxBackupSizePerUser :: !(Maybe Word64),
    envAutoBackupLooperEnv :: !LooperEnvironment,
    envBackupGarbageCollectionLooperEnv :: !LooperEnvironment,
    envAdmin :: !(Maybe Username)
  }
  deriving (Show, Eq, Generic)

data Configuration = Configuration
  { confLogLevel :: !(Maybe LogLevel),
    confUUIDFile :: !(Maybe FilePath),
    confDatabaseFile :: !(Maybe FilePath),
    confSigningKeyFile :: !(Maybe FilePath),
    confPort :: !(Maybe Int),
    confMaxBackupsPerUser :: !(Maybe Word),
    confMaxBackupSizePerUser :: !(Maybe Word64),
    confAutoBackupLooperConfiguration :: !(Maybe LooperConfiguration),
    confBackupGarbageCollectionLooperConfiguration :: !(Maybe LooperConfiguration),
    confAdmin :: !(Maybe Username)
  }
  deriving (Show, Eq, Generic)

instance FromJSON Configuration where
  parseJSON = viaYamlSchema

instance YamlSchema Configuration where
  yamlSchema = objectParser "Configuration" configurationObjectParser

configurationObjectParser :: ObjectParser Configuration
configurationObjectParser =
  Configuration
    <$> optionalFieldWith "log-level" "The minimal severity for log messages" (maybeParser parseLogLevel yamlSchema)
    <*> optionalField "uuid-file" "The file in which to store the server uuid"
    <*> optionalField "database-file" "The file in which to store the database"
    <*> optionalField "signing-key-file" "The file in which to store signing key for JWT tokens"
    <*> optionalField "port" "The port on which to serve api requests"
    <*> optionalField "max-backups-per-user" "The maximum number of backups per user"
    <*> optionalField "max-backup-size-per-user" "The maximum number of bytes that backups can take up per user"
    <*> optionalField "auto-backup" "The configuration for the automatic backup looper"
    <*> optionalField "backup-garbage-collector" "The configuration for the automatic backup garbage collection looper"
    <*> optionalField "admin" "The username of the user who will have admin rights"

newtype Dispatch
  = DispatchServe ServeSettings
  deriving (Show, Eq, Generic)

data ServeSettings = ServeSettings
  { serveSetLogLevel :: !LogLevel,
    serveSetUUIDFile :: !(Path Abs File),
    serveSetDatabaseFile :: !(Path Abs File),
    serveSetSigningKeyFile :: !(Path Abs File),
    serveSetPort :: !Int,
    serveSetMaxBackupsPerUser :: !(Maybe Word),
    serveSetMaxBackupSizePerUser :: !(Maybe Word64),
    serveSetAutoBackupLooperSettings :: !LooperSettings,
    serveSetBackupGarbageCollectionLooperSettings :: !LooperSettings,
    serveSetAdmin :: !(Maybe Username)
  }
  deriving (Show, Eq, Generic)

data Settings
  = Settings
  deriving (Show, Eq, Generic)

parseLogLevel :: String -> Maybe LogLevel
parseLogLevel s = readMaybe $ "Level" <> s

renderLogLevel :: LogLevel -> String
renderLogLevel = drop 5 . show
