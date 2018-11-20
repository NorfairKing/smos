{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.OptParse.Types where

import Data.Yaml as Yaml

data Flags = Flags
    { flagWorkflowDir :: Maybe FilePath
    } deriving (Show, Eq)

data Environment = Environment
    { envWorkflowDir :: Maybe FilePath
    } deriving (Show, Eq)

data Configuration = Configuration
    { confWorkflowDir :: Maybe FilePath
    } deriving (Show, Eq)

instance FromJSON Configuration where
    parseJSON =
        withObject "Configuration" $ \o ->
            Configuration <$> o .:? "workflow-dir"
