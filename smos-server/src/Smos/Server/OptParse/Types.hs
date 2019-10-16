{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Server.OptParse.Types where

import GHC.Generics (Generic)

import Control.Monad.Logger
import Data.Yaml as Yaml
import Text.Read

import Path

data Arguments =
  Arguments Command Flags
  deriving (Show, Eq)

data Instructions =
  Instructions Dispatch Settings

newtype Command =
  CommandServe ServeFlags
  deriving (Show, Eq)

data ServeFlags =
  ServeFlags
    { serveFlagLogLevel :: Maybe LogLevel
    , serveFlagUUIDFile :: Maybe FilePath
    , serveFlagDatabaseFile :: Maybe FilePath
    , serveFlagPort :: Maybe Int
    }
  deriving (Show, Eq)

newtype Flags =
  Flags
    { flagConfigFile :: Maybe FilePath
    }
  deriving (Show, Eq, Generic)

data Environment =
  Environment
    { envConfigFile :: Maybe FilePath
    , envLogLevel :: Maybe LogLevel
    , envUUIDFile :: Maybe FilePath
    , envDatabaseFile :: Maybe FilePath
    , envPort :: Maybe Int
    }
  deriving (Show, Eq, Generic)

data Configuration =
  Configuration
    { confLogLevel :: Maybe LogLevel
    , confUUIDFile :: Maybe FilePath
    , confDatabaseFile :: Maybe FilePath
    , confPort :: Maybe Int
    }
  deriving (Show, Eq, Generic)

instance FromJSON Configuration where
  parseJSON =
    withObject "Configuration" $ \o ->
      Configuration <$>
      (do ms <- o .:? "log-level"
          case ms of
            Nothing -> pure Nothing
            Just s ->
              case parseLogLevel s of
                Nothing -> fail $ "Unknown log level: " <> s
                Just ll -> pure $ Just ll) <*>
      o .:? "uuid-file" <*>
      o .:? "database-file" <*>
      o .:? "port"

newtype Dispatch =
  DispatchServe ServeSettings
  deriving (Show, Eq, Generic)

data ServeSettings =
  ServeSettings
    { serveSetLogLevel :: LogLevel
    , serveSetUUIDFile :: Path Abs File
    , serveSetDatabaseFile :: Path Abs File
    , serveSetPort :: Int
    }
  deriving (Show, Eq, Generic)

data Settings =
  Settings
  deriving (Show, Eq, Generic)

parseLogLevel :: String -> Maybe LogLevel
parseLogLevel s = readMaybe $ "Level" <> s

renderLogLevel :: LogLevel -> String
renderLogLevel = drop 5 . show
