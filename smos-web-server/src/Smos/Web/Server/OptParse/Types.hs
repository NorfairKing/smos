{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Web.Server.OptParse.Types where

import Control.Monad.Logger
import Data.Yaml as Yaml
import GHC.Generics (Generic)
import Path
import Servant.Client
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
        serveFlagPort :: !(Maybe Int),
        serveFlagDocsUrl :: !(Maybe String),
        serveFlagDataDir :: !(Maybe FilePath)
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
        envWebServerEnv :: !WebServerEnvironment
      }
  deriving (Show, Eq, Generic)

data WebServerEnvironment
  = WebServerEnvironment
      { envLogLevel :: !(Maybe LogLevel),
        envPort :: !(Maybe Int),
        envDocsUrl :: !(Maybe String),
        envDataDir :: !(Maybe FilePath)
      }
  deriving (Show, Eq, Generic)

data Configuration
  = Configuration
      { confAPIConfiguration :: !API.Configuration,
        confLogLevel :: !(Maybe LogLevel),
        confPort :: !(Maybe Int),
        confDocsUrl :: !(Maybe String),
        confDataDir :: !(Maybe FilePath)
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
        <*> optionalField "docs-url" "The url for the documentation site to refer to"
        <*> optionalField "data-dir" "The directory to store workflows during editing"

newtype Dispatch
  = DispatchServe ServeSettings
  deriving (Show, Eq, Generic)

data ServeSettings
  = ServeSettings
      { serveSetAPISettings :: !API.ServeSettings,
        serveSetLogLevel :: !LogLevel,
        serveSetPort :: !Int,
        serveSetDocsUrl :: !(Maybe BaseUrl),
        serveSetDataDir :: !(Path Abs Dir)
      }
  deriving (Show, Eq, Generic)

data Settings
  = Settings
  deriving (Show, Eq, Generic)
