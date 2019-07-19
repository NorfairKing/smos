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
    { flagConfigFile :: Maybe FilePath,flagWorkflowDir :: Maybe FilePath
    , flagArchiveDir :: Maybe FilePath
    }
  deriving (Show, Eq, Generic)

data Environment =
  Environment
    { envConfigFile :: Maybe FilePath,envWorkflowDir :: Maybe FilePath
    , envArchiveDir :: Maybe FilePath
    }
  deriving (Show, Eq, Generic)

data Configuration =
  Configuration
    { confWorkflowDir :: Maybe FilePath
    , confArchiveDir :: Maybe FilePath
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
                 DirInHome rd -> "~/" <> fromRelDir rd
                 DirAbsolute ad -> fromAbsDir ad
    , confArchiveDir =
        if smosReportConfigArchiveFileSpec == defaultArchiveDirSpec
          then Nothing
          else Just $
               case smosReportConfigArchiveFileSpec of
                 ArchiveInWorkflow ard -> fromRelDir ard
                 ArchiveInHome ard -> "~/" <> fromRelDir ard
                 ArchiveAbsolute aad -> fromAbsDir aad
    }

instance Validity Configuration

instance ToJSON Configuration where
  toJSON Configuration {..} =
    object ["workflow-dir" .= confWorkflowDir, "archive-dir" .= confArchiveDir]

instance FromJSON Configuration where
  parseJSON =
    withObject "Configuration" $ \o ->
      Configuration <$> o .:? "workflow-dir" <*> o .:? "archive-dir"
