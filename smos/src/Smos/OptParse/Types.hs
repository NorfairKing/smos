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
    } deriving (Show, Eq)

instance FromJSON Configuration where
    parseJSON v = Configuration <$> parseJSON v

configurationType :: Dhall.Type Configuration
configurationType = Configuration <$> Report.configurationType

data Instructions =
    Instructions (Path Abs File)
                 SmosConfig
