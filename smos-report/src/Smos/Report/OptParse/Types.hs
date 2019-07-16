{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.OptParse.Types where

import GHC.Generics (Generic)

import Data.Validity

import Data.Yaml as Yaml

data Flags =
  Flags
    { flagWorkflowDir :: Maybe FilePath
    }
  deriving (Show, Eq)

data Environment =
  Environment
    { envWorkflowDir :: Maybe FilePath
    }
  deriving (Show, Eq)

data Configuration =
  Configuration
    { confWorkflowDir :: Maybe FilePath
    }
  deriving (Show, Eq, Generic)

instance Validity Configuration

instance ToJSON Configuration where
  toJSON Configuration {..} = object ["workflow-dir" .= confWorkflowDir]

instance FromJSON Configuration where
  parseJSON = withObject "Configuration" $ \o -> Configuration <$> o .:? "workflow-dir"
