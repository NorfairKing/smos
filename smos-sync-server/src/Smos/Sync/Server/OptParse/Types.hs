{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Sync.Server.OptParse.Types where

import GHC.Generics (Generic)

import Data.Yaml as Yaml

data Arguments =
  Arguments Command Flags
  deriving (Show, Eq)

data Instructions =
  Instructions Dispatch Settings

data Command =
  CommandServe ServeFlags
  deriving (Show, Eq)

data ServeFlags =
  ServeFlags
    { serveFlagPort :: Maybe Int
    }
  deriving (Show, Eq)

data Flags =
  Flags
    { flagConfigFile :: Maybe FilePath}
  deriving (Show, Eq, Generic)

data Environment =
  Environment
    { envConfigFile :: Maybe FilePath
    , envPort :: Maybe Int
    }
  deriving (Show, Eq, Generic)

data Configuration =
  Configuration
    { confPort :: Maybe Int
    }
  deriving (Show, Eq, Generic)

instance FromJSON Configuration where
  parseJSON = withObject "Configuration" $ \o -> Configuration <$> o .: "port"

data Dispatch =
  DispatchServe ServeSettings
  deriving (Show, Eq, Generic)

data ServeSettings =
  ServeSettings
    { serveSetPort :: Int
    }
  deriving (Show, Eq, Generic)

data Settings =
  Settings
  deriving (Show, Eq, Generic)
