{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.OptParse.Types where

import Import

import Smos.Types

import Data.Yaml as Yaml
import Dhall

data Arguments =
    Arguments FilePath
              Flags

data Flags = Flags
    { flagConfigFile :: Maybe FilePath
    , flagWorkflowDir :: Maybe FilePath
    } deriving (Show, Eq)

data Environment = Environment
    { envConfigFile :: Maybe FilePath
    , envWorkflowDir :: Maybe FilePath
    } deriving (Show, Eq)

data Configuration = Configuration
    { confWorkflowDir :: Maybe FilePath
    } deriving (Show, Eq)

instance FromJSON Configuration where
    parseJSON =
        withObject "Configuration" $ \o ->
            Configuration <$> o .:? "workflow-dir"

configurationDefaults :: Text
configurationDefaults = "{ workflowDir = [] : Optional Text }"

configurationType :: Dhall.Type Configuration
configurationType =
    Dhall.record
        (Configuration <$> Dhall.field "workflowDir" (Dhall.maybe Dhall.string))

data Instructions =
    Instructions (Path Abs File)
                 SmosConfig
