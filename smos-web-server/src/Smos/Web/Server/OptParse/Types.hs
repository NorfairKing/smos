{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Web.Server.OptParse.Types where

import Control.Monad.Logger
import Data.Yaml as Yaml
import GHC.Generics (Generic)
import qualified Smos.Server.OptParse.Types as API
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
      { serveFlagAPIFlags :: !API.ServeFlags,
        serveFlagLogLevel :: !(Maybe LogLevel),
        serveFlagPort :: !(Maybe Int)
      }
  deriving (Show, Eq)

data Flags
  = Flags
      { flagAPIFlags :: !API.Flags
      }
  deriving (Show, Eq, Generic)

data Environment
  = Environment
      { envAPIEnv :: !API.Environment,
        envLogLevel :: !(Maybe LogLevel),
        envPort :: !(Maybe Int)
      }
  deriving (Show, Eq, Generic)

data Configuration
  = Configuration
      { confAPIConfiguration :: !API.Configuration,
        confLogLevel :: !(Maybe LogLevel),
        confPort :: !(Maybe Int)
      }
  deriving (Show, Eq, Generic)

instance FromJSON Configuration where
  parseJSON = viaYamlSchema

instance YamlSchema Configuration where
  yamlSchema =
    objectParser "Configuration" $
      Configuration
        <$> API.configurationObjectParser
        <*> optionalFieldWith "web-log-level" "The minimal severity for log messages" (maybeParser API.parseLogLevel yamlSchema)
        <*> optionalField "web-port" "The port on which to serve web requests"

newtype Dispatch
  = DispatchServe ServeSettings
  deriving (Show, Eq, Generic)

data ServeSettings
  = ServeSettings
      { serveSetAPISettings :: !API.ServeSettings,
        serveSetLogLevel :: LogLevel,
        serveSetPort :: Int
      }
  deriving (Show, Eq, Generic)

data Settings
  = Settings
  deriving (Show, Eq, Generic)
