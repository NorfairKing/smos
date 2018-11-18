{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.OptParse.Types where

import Import

import Data.Yaml as Yaml
import Dhall

import qualified Smos.Report.OptParse.Types as Report

import Smos.Types

data Arguments =
    Arguments FilePath
              Flags

data Flags = Flags
    { flagConfigFile :: Maybe FilePath
    , flagReportFlags :: Report.Flags
    } deriving (Show, Eq)

data Environment = Environment
    { envConfigFile :: Maybe FilePath
    , envReportEnv :: Report.Environment
    } deriving (Show, Eq)

data Configuration = Configuration
    { confReportConf :: Report.Configuration
    , confKeybindingsConf :: KeybindingsConfiguration
    } deriving (Show, Eq)

instance FromJSON Configuration where
    parseJSON v = Configuration <$> parseJSON v <*> parseJSON v

configurationDefaults :: Text
configurationDefaults =
    Report.configurationDefaults <> "// { reset = [ False ] : Optional Bool }"

configurationType :: Dhall.Type Configuration
configurationType =
    Dhall.record
        (Configuration <$> Report.configurationRecordType <*>
         keybindingsConfigurationRecordType)

data KeybindingsConfiguration = KeybindingsConfiguration
    { confReset :: Maybe Bool
    } deriving (Show, Eq)

instance FromJSON KeybindingsConfiguration where
    parseJSON = withObject "KeybindingsConfiguration" $ \o -> o .: "reset"

keybindingsConfigurationRecordType :: Dhall.RecordType KeybindingsConfiguration
keybindingsConfigurationRecordType =
    KeybindingsConfiguration <$> Dhall.field "reset" (Dhall.maybe Dhall.bool)

data Instructions =
    Instructions (Path Abs File)
                 SmosConfig
