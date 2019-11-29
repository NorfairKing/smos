{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.OptParse.Types where

import GHC.Generics (Generic)

import Data.Map (Map)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Validity
import Data.Yaml as Yaml
import Path

import Smos.Report.Config
import Smos.Report.Filter

data Flags =
  Flags
    { flagConfigFile :: Maybe FilePath
    , flagWorkflowDir :: Maybe FilePath
    , flagArchiveDir :: Maybe FilePath
    , flagProjectsDir :: Maybe FilePath
    , flagArchivedProjectsDir :: Maybe FilePath
    }
  deriving (Show, Eq, Generic)

data Environment =
  Environment
    { envConfigFile :: Maybe FilePath
    , envWorkflowDir :: Maybe FilePath
    , envArchiveDir :: Maybe FilePath
    , envProjectsDir :: Maybe FilePath
    , envArchivedProjectsDir :: Maybe FilePath
    }
  deriving (Show, Eq, Generic)

data Configuration =
  Configuration
    { confWorkflowDir :: Maybe Text
    , confArchiveDir :: Maybe Text
    , confProjectsDir :: Maybe Text
    , confArchivedProjectsDir :: Maybe Text
    , confWorkBaseFilter :: Maybe EntryFilter
    , confContexts :: Maybe (Map ContextName EntryFilter)
    }
  deriving (Show, Eq, Generic)

backToConfiguration :: SmosReportConfig -> Configuration
backToConfiguration SmosReportConfig {..} =
  Configuration
    { confWorkflowDir =
        if smosReportConfigWorkflowFileSpec == defaultWorkflowDirSpec
          then Nothing
          else Just $
               T.pack $
               case smosReportConfigWorkflowFileSpec of
                 DirInHome rd -> "~/" <> fromRelDir rd
                 DirAbsolute ad -> fromAbsDir ad
    , confArchiveDir =
        if smosReportConfigArchiveFileSpec == defaultArchiveDirSpec
          then Nothing
          else Just $
               T.pack $
               case smosReportConfigArchiveFileSpec of
                 ArchiveInWorkflow ard -> fromRelDir ard
                 ArchiveInHome ard -> "~/" <> fromRelDir ard
                 ArchiveAbsolute aad -> fromAbsDir aad
    , confProjectsDir =
        if smosReportConfigProjectsFileSpec == defaultProjectsDirSpec
          then Nothing
          else Just $
               T.pack $
               case smosReportConfigProjectsFileSpec of
                 ProjectsInWorkflow ard -> fromRelDir ard
                 ProjectsInHome ard -> "~/" <> fromRelDir ard
                 ProjectsAbsolute aad -> fromAbsDir aad
    , confArchivedProjectsDir =
        if smosReportConfigArchivedProjectsFileSpec == defaultArchivedProjectsDirSpec
          then Nothing
          else Just $
               T.pack $
               case smosReportConfigArchivedProjectsFileSpec of
                 ArchivedProjectsInArchive ard -> fromRelDir ard
                 ArchivedProjectsInHome ard -> "~/" <> fromRelDir ard
                 ArchivedProjectsAbsolute aad -> fromAbsDir aad
    , confWorkBaseFilter =
        if smosReportConfigWorkBaseFilter == Just defaultWorkBaseFilter
          then Nothing
          else Just defaultWorkBaseFilter
    , confContexts = Just smosReportConfigContexts
    }

instance Validity Configuration

instance ToJSON Configuration where
  toJSON Configuration {..} =
    object
      [ "workflow-dir" .= confWorkflowDir
      , "archive-dir" .= confArchiveDir
      , "projects-dir" .= confProjectsDir
      , "archived-projects-dir" .= confArchivedProjectsDir
      , "work-filter" .= confWorkBaseFilter
      , "contexts" .= confContexts
      ]

instance FromJSON Configuration where
  parseJSON =
    withObject "Configuration" $ \o ->
      Configuration <$> o .:? "workflow-dir" <*> o .:? "archive-dir" <*> o .:? "projects-dir" <*>
      o .:? "archived-projects-dir" <*>
      o .:? "work-filter" <*>
      o .:? "contexts"
