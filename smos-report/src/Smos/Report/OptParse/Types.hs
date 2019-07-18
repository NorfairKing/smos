{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.OptParse.Types where

import GHC.Generics (Generic)

import Data.Validity
import Data.Yaml as Yaml
import Path

import Smos.Report.Config

data Flags =
  Flags
    { flagWorkflowDir :: Maybe FilePath
    }
  deriving (Show, Eq, Generic)

data Environment =
  Environment
    { envWorkflowDir :: Maybe FilePath
    }
  deriving (Show, Eq, Generic)

data Configuration =
  Configuration
    { confWorkflowDir :: Maybe FilePath
    }
  deriving (Show, Eq, Generic)

backToConfiguration :: SmosReportConfig -> Configuration
backToConfiguration SmosReportConfig {..} =
  Configuration
    { confWorkflowDir =
        if smosReportConfigAgendaFileSpec == defaultWorkflowDirSpec
          then Nothing
          else Just $
               case smosReportConfigAgendaFileSpec of
                 DirInHome rd ->  "~/" <> fromRelDir rd
                 DirAbsolute ad ->  fromAbsDir ad
    }

instance Validity Configuration

instance ToJSON Configuration where
  toJSON Configuration {..} = object ["workflow-dir" .= confWorkflowDir]

instance FromJSON Configuration where
  parseJSON = withObject "Configuration" $ \o -> Configuration <$> o .:? "workflow-dir"
