{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Server.OptParse.Types where

import Control.Monad.Logger
import Data.Yaml as Yaml
import GHC.Generics (Generic)
import Path
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

data ServeFlags
  = ServeFlags
      { serveFlagLogLevel :: Maybe LogLevel,
        serveFlagUUIDFile :: Maybe FilePath,
        serveFlagDatabaseFile :: Maybe FilePath,
        serveFlagPort :: Maybe Int
      }
  deriving (Show, Eq)

newtype Flags
  = Flags
      { flagConfigFile :: Maybe FilePath
      }
  deriving (Show, Eq, Generic)

data Environment
  = Environment
      { envConfigFile :: Maybe FilePath,
        envLogLevel :: Maybe LogLevel,
        envUUIDFile :: Maybe FilePath,
        envDatabaseFile :: Maybe FilePath,
        envPort :: Maybe Int
      }
  deriving (Show, Eq, Generic)

data Configuration
  = Configuration
      { confLogLevel :: Maybe LogLevel,
        confUUIDFile :: Maybe FilePath,
        confDatabaseFile :: Maybe FilePath,
        confPort :: Maybe Int
      }
  deriving (Show, Eq, Generic)

instance FromJSON Configuration where
  parseJSON = viaYamlSchema

instance YamlSchema Configuration where
  yamlSchema = objectParser "Configuration" configurationObjectParser

configurationObjectParser :: ObjectParser Configuration
configurationObjectParser =
  Configuration
    <$> optionalFieldWith "api-log-level" "The minimal severity for log messages" (maybeParser parseLogLevel yamlSchema)
    <*> optionalField "uuid-file" "The file in which to store the server uuid"
    <*> optionalField "database-file" "The file in which to store the database"
    <*> optionalField "api-port" "The port on which to serve api requests"

newtype Dispatch
  = DispatchServe ServeSettings
  deriving (Show, Eq, Generic)

data ServeSettings
  = ServeSettings
      { serveSetLogLevel :: LogLevel,
        serveSetUUIDFile :: Path Abs File,
        serveSetDatabaseFile :: Path Abs File,
        serveSetPort :: Int
      }
  deriving (Show, Eq, Generic)

data Settings
  = Settings
  deriving (Show, Eq, Generic)

parseLogLevel :: String -> Maybe LogLevel
parseLogLevel s = readMaybe $ "Level" <> s

renderLogLevel :: LogLevel -> String
renderLogLevel = drop 5 . show
