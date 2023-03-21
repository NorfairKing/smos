{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.OptParse.Types where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import GHC.Generics (Generic)
import Path
import Smos.Data
import Smos.Report.Config
import Smos.Report.Filter
import Smos.Report.Projection
import Smos.Report.Sorter
import Smos.Report.Time

data Flags = Flags
  { flagDirectoryFlags :: DirectoryFlags
  }
  deriving (Show, Eq, Generic)

data DirectoryFlags = DirectoryFlags
  { dirFlagWorkflowDir :: Maybe FilePath,
    dirFlagArchiveDir :: Maybe FilePath,
    dirFlagProjectsDir :: Maybe FilePath,
    dirFlagArchivedProjectsDir :: Maybe FilePath
  }
  deriving (Show, Eq, Generic)

data Environment = Environment
  { envDirectoryEnvironment :: DirectoryEnvironment
  }
  deriving (Show, Eq, Generic)

emptyEnvironment :: Environment
emptyEnvironment =
  Environment
    { envDirectoryEnvironment = emptyDirectoryEnvironment
    }

emptyDirectoryEnvironment :: DirectoryEnvironment
emptyDirectoryEnvironment =
  DirectoryEnvironment
    { dirEnvWorkflowDir = Nothing,
      dirEnvArchiveDir = Nothing,
      dirEnvProjectsDir = Nothing,
      dirEnvArchivedProjectsDir = Nothing
    }

data DirectoryEnvironment = DirectoryEnvironment
  { dirEnvWorkflowDir :: Maybe FilePath,
    dirEnvArchiveDir :: Maybe FilePath,
    dirEnvProjectsDir :: Maybe FilePath,
    dirEnvArchivedProjectsDir :: Maybe FilePath
  }
  deriving (Show, Eq, Generic)

data Configuration = Configuration
  { confDirectoryConf :: !DirectoryConfiguration,
    confWaitingReportConf :: !(Maybe WaitingReportConfiguration),
    confStuckReportConf :: !(Maybe StuckReportConfiguration),
    confWorkReportConf :: !(Maybe WorkReportConfiguration)
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via (Autodocodec Configuration)

instance Validity Configuration

instance HasCodec Configuration where
  codec = object "Configuration" objectCodec

instance HasObjectCodec Configuration where
  objectCodec =
    Configuration
      <$> objectCodec .= confDirectoryConf
      <*> optionalFieldOrNull "waiting" "The waiting report configuration" .= confWaitingReportConf
      <*> optionalFieldOrNull "stuck" "The stuck projects report configuration" .= confStuckReportConf
      <*> optionalFieldOrNull "work" "The work report configuration" .= confWorkReportConf

defaultConfiguration :: Configuration
defaultConfiguration =
  Configuration
    { confDirectoryConf = defaultDirectoryConfiguration,
      confWorkReportConf = Nothing,
      confStuckReportConf = Nothing,
      confWaitingReportConf = Nothing
    }

backToConfiguration :: SmosReportConfig -> Configuration
backToConfiguration SmosReportConfig {..} =
  Configuration
    { confDirectoryConf = backToDirectoryConfiguration smosReportConfigDirectoryConfig,
      confWaitingReportConf =
        if smosReportConfigWaitingConfig == defaultWaitingReportConfig
          then Nothing
          else Just $ backToWaitingReportConfiguration smosReportConfigWaitingConfig,
      confStuckReportConf =
        if smosReportConfigStuckConfig == defaultStuckReportConfig
          then Nothing
          else Just $ backToStuckReportConfiguration smosReportConfigStuckConfig,
      confWorkReportConf =
        if smosReportConfigWorkConfig == defaultWorkReportConfig
          then Nothing
          else Just $ backToWorkReportConfiguration smosReportConfigWorkConfig
    }

data DirectoryConfiguration = DirectoryConfiguration
  { directoryConfWorkflowDir :: !(Maybe Text),
    directoryConfArchiveDir :: !(Maybe Text),
    directoryConfProjectsDir :: !(Maybe Text),
    directoryConfArchivedProjectsDir :: !(Maybe Text)
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via (Autodocodec DirectoryConfiguration)

instance Validity DirectoryConfiguration

instance HasCodec DirectoryConfiguration where
  codec = object "DirectoryConfiguration" objectCodec

instance HasObjectCodec DirectoryConfiguration where
  objectCodec =
    DirectoryConfiguration
      <$> optionalFieldOrNull "workflow-dir" "The workflow directory" .= directoryConfWorkflowDir
      <*> optionalFieldOrNull "archive-dir" "The archive directory" .= directoryConfArchiveDir
      <*> optionalFieldOrNull "projects-dir" "The projects directory" .= directoryConfProjectsDir
      <*> optionalFieldOrNull "archived-projects-dir" "The archived projects directory" .= directoryConfArchivedProjectsDir

defaultDirectoryConfiguration :: DirectoryConfiguration
defaultDirectoryConfiguration =
  DirectoryConfiguration
    { directoryConfWorkflowDir = Nothing,
      directoryConfArchiveDir = Nothing,
      directoryConfProjectsDir = Nothing,
      directoryConfArchivedProjectsDir = Nothing
    }

backToDirectoryConfiguration :: DirectoryConfig -> DirectoryConfiguration
backToDirectoryConfiguration DirectoryConfig {..} =
  DirectoryConfiguration
    { directoryConfWorkflowDir =
        if directoryConfigWorkflowFileSpec == defaultWorkflowDirSpec
          then Nothing
          else Just $
            T.pack $
              case directoryConfigWorkflowFileSpec of
                WorkflowInHome rd -> "~/" <> fromRelDir rd
                AbsoluteWorkflow ad -> fromAbsDir ad,
      directoryConfArchiveDir =
        if directoryConfigArchiveFileSpec == defaultArchiveDirSpec
          then Nothing
          else Just $
            T.pack $
              case directoryConfigArchiveFileSpec of
                ArchiveInWorkflow ard -> fromRelDir ard
                ArchiveInHome ard -> "~/" <> fromRelDir ard
                ArchiveAbsolute aad -> fromAbsDir aad,
      directoryConfProjectsDir =
        if directoryConfigProjectsFileSpec == defaultProjectsDirSpec
          then Nothing
          else Just $
            T.pack $
              case directoryConfigProjectsFileSpec of
                ProjectsInWorkflow ard -> fromRelDir ard
                ProjectsInHome ard -> "~/" <> fromRelDir ard
                ProjectsAbsolute aad -> fromAbsDir aad,
      directoryConfArchivedProjectsDir =
        if directoryConfigArchivedProjectsFileSpec == defaultArchivedProjectsDirSpec
          then Nothing
          else Just $
            T.pack $
              case directoryConfigArchivedProjectsFileSpec of
                ArchivedProjectsInArchive ard -> fromRelDir ard
                ArchivedProjectsInHome ard -> "~/" <> fromRelDir ard
                ArchivedProjectsAbsolute aad -> fromAbsDir aad
    }

data WaitingReportConfiguration = WaitingReportConfiguration
  { waitingReportConfThreshold :: !(Maybe Time)
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via (Autodocodec WaitingReportConfiguration)

instance Validity WaitingReportConfiguration

instance HasCodec WaitingReportConfiguration where
  codec =
    object "WaitingReportConfiguration" $
      WaitingReportConfiguration
        <$> optionalFieldOrNull "threshold" "waiting report threshold to consider waiting entries 'overdue'" .= waitingReportConfThreshold

backToWaitingReportConfiguration :: WaitingReportConfig -> WaitingReportConfiguration
backToWaitingReportConfiguration WaitingReportConfig {..} =
  WaitingReportConfiguration
    { waitingReportConfThreshold =
        if waitingReportConfigThreshold == defaultWaitingThreshold
          then Nothing
          else Just defaultWaitingThreshold
    }

data StuckReportConfiguration = StuckReportConfiguration
  { stuckReportConfThreshold :: !(Maybe Time)
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via (Autodocodec StuckReportConfiguration)

instance Validity StuckReportConfiguration

instance HasCodec StuckReportConfiguration where
  codec =
    object "StuckReportConfiguration" $
      StuckReportConfiguration
        <$> optionalFieldOrNull "threshold" "stuck report threshold to consider stuck projects 'overdue'" .= stuckReportConfThreshold

backToStuckReportConfiguration :: StuckReportConfig -> StuckReportConfiguration
backToStuckReportConfiguration StuckReportConfig {..} =
  StuckReportConfiguration
    { stuckReportConfThreshold =
        if stuckReportConfigThreshold == defaultStuckThreshold
          then Nothing
          else Just defaultStuckThreshold
    }

data WorkReportConfiguration = WorkReportConfiguration
  { workReportConfBaseFilter :: !(Maybe EntryFilter),
    workReportConfChecks :: !(Maybe (Set EntryFilter)),
    workReportConfContexts :: !(Maybe (Map ContextName EntryFilter)),
    workReportConfTimeFilterProperty :: Maybe PropertyName,
    workReportConfProjection :: Maybe (NonEmpty Projection),
    workReportConfSorter :: Maybe Sorter
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec WorkReportConfiguration)

instance Validity WorkReportConfiguration

instance HasCodec WorkReportConfiguration where
  codec =
    object "WorkReportConfiguration" $
      WorkReportConfiguration
        <$> optionalFieldOrNull "base-filter" "The base work filter" .= workReportConfBaseFilter
        <*> optionalFieldOrNull "checks" "Checks for the work report" .= workReportConfChecks
        <*> optionalFieldOrNull "contexts" "Contexts for the work report" .= workReportConfContexts
        <*> optionalFieldOrNull "time-filter" "The property to use to filter by time" .= workReportConfTimeFilterProperty
        <*> optionalFieldOrNull "columns" "The columns in the report" .= workReportConfProjection
        <*> optionalFieldOrNull "sorter" "The sorter to use to sort the rows" .= workReportConfSorter

defaultWorkReportConfiguration :: WorkReportConfiguration
defaultWorkReportConfiguration =
  WorkReportConfiguration
    { workReportConfBaseFilter = Nothing,
      workReportConfChecks = Nothing,
      workReportConfContexts = Nothing,
      workReportConfTimeFilterProperty = Nothing,
      workReportConfProjection = Nothing,
      workReportConfSorter = Nothing
    }

backToWorkReportConfiguration :: WorkReportConfig -> WorkReportConfiguration
backToWorkReportConfiguration WorkReportConfig {..} =
  WorkReportConfiguration
    { workReportConfBaseFilter =
        if workReportConfigBaseFilter == Just defaultWorkBaseFilter
          then Nothing
          else Just defaultWorkBaseFilter,
      workReportConfChecks = Just workReportConfigChecks,
      workReportConfContexts = Just workReportConfigContexts,
      workReportConfTimeFilterProperty = workReportConfigTimeProperty,
      workReportConfProjection = Just workReportConfigProjection,
      workReportConfSorter = workReportConfigSorter
    }
