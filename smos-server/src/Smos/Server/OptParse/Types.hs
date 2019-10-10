{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Server.OptParse.Types where

import GHC.Generics (Generic)

import Data.Yaml as Yaml

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
    { serveFlagUUIDFile :: Maybe FilePath
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
    , envUUIDFile :: Maybe FilePath
    , envDatabaseFile :: Maybe FilePath
    , envPort :: Maybe Int
    }
  deriving (Show, Eq, Generic)

data Configuration =
  Configuration
    { confUUIDFile :: Maybe FilePath
    , confDatabaseFile :: Maybe FilePath
    , confPort :: Maybe Int
    }
  deriving (Show, Eq, Generic)

instance FromJSON Configuration where
  parseJSON =
    withObject "Configuration" $ \o ->
      Configuration <$> o .:? "uuid-file" <*> o .:? "database-file" <*> o .:? "port"

newtype Dispatch =
  DispatchServe ServeSettings
  deriving (Show, Eq, Generic)

data ServeSettings =
  ServeSettings
    { serveSetUUIDFile :: Path Abs File
    , serveSetDatabaseFile :: Path Abs File
    , serveSetPort :: Int
    }
  deriving (Show, Eq, Generic)

data Settings =
  Settings
  deriving (Show, Eq, Generic)
