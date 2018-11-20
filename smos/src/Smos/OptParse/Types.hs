{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.OptParse.Types where

import Import

import Data.Yaml as Yaml

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
    } deriving (Show, Eq, Generic)

instance FromJSON Configuration where
    parseJSON v = Configuration <$> parseJSON v <*> parseJSON v

data KeybindingsConfiguration = KeybindingsConfiguration
    { confReset :: Maybe Bool
    , confKeyConfig :: Maybe KeyConfig
    } deriving (Show, Eq, Generic)

instance FromJSON KeybindingsConfiguration where
    parseJSON =
        withObject "KeybindingsConfiguration" $ \o ->
            KeybindingsConfiguration <$> o .:? "reset" <*> o .:? "help"

data KeyConfig = KeyConfig
    { keyConfigCatchAll :: Maybe ActionName
    } deriving (Show, Eq, Generic)

instance FromJSON KeyConfig where
    parseJSON = withObject "KeyConfig" $ \o -> KeyConfig <$> o .:? "catch-all"

data Instructions =
    Instructions (Path Abs File)
                 SmosConfig
