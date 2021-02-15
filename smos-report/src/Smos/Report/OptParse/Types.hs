{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.OptParse.Types where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Yaml as Yaml
import GHC.Generics (Generic)
import Path
import Smos.Data
import Smos.Report.Config
import Smos.Report.Filter
import Smos.Report.Projection
import Smos.Report.Sorter
import YamlParse.Applicative as YamlParse

data Flags = Flags
  { flagDirectoryFlags :: DirectoryFlags
  }
  deriving (Show, Eq, Generic)

data FlagsWithConfigFile a = FlagsWithConfigFile
  { flagWithConfigFile :: Maybe FilePath,
    flagWithRestFlags :: a
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

data EnvWithConfigFile a = EnvWithConfigFile
  { envWithConfigFile :: Maybe FilePath,
    envWithRestEnv :: a
  }
  deriving (Show, Eq, Generic)

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
  deriving (Show, Eq, Generic)

instance Validity Configuration

instance ToJSON Configuration where
  toJSON Configuration {..} =
    object $
      directoryConfigurationToObject confDirectoryConf
        ++ [ "waiting" .= confWaitingReportConf,
             "stuck" .= confStuckReportConf,
             "work" .= confWorkReportConf
           ]

instance FromJSON Configuration where
  parseJSON = viaYamlSchema

instance YamlSchema Configuration where
  yamlSchema =
    objectParser "Configuration" $
      Configuration
        <$> directoryConfigurationObjectParser
        <*> optionalField "waiting" "The waiting report configuration"
        <*> optionalField "stuck" "The stuck projects report configuration"
        <*> optionalField "work" "The work report configuration"

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
  deriving (Show, Eq, Generic)

instance Validity DirectoryConfiguration

instance ToJSON DirectoryConfiguration where
  toJSON =
    object . directoryConfigurationToObject

directoryConfigurationToObject :: DirectoryConfiguration -> [(Text, Yaml.Value)]
directoryConfigurationToObject DirectoryConfiguration {..} =
  [ "workflow-dir" .= directoryConfWorkflowDir,
    "archive-dir" .= directoryConfArchiveDir,
    "projects-dir" .= directoryConfProjectsDir,
    "archived-projects-dir" .= directoryConfArchivedProjectsDir
  ]

instance FromJSON DirectoryConfiguration where
  parseJSON = viaYamlSchema

instance YamlSchema DirectoryConfiguration where
  yamlSchema =
    objectParser "DirectoryConfiguration" directoryConfigurationObjectParser

directoryConfigurationObjectParser :: YamlParse.ObjectParser DirectoryConfiguration
directoryConfigurationObjectParser =
  DirectoryConfiguration
    <$> optionalField "workflow-dir" "The workflow directory"
    <*> optionalField "archive-dir" "The archive directory"
    <*> optionalField "projects-dir" "The projects directory"
    <*> optionalField "archived-projects-dir" "The archived projects directory"

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
  { waitingReportConfThreshold :: !(Maybe Word)
  }
  deriving (Show, Eq, Generic)

instance Validity WaitingReportConfiguration

instance ToJSON WaitingReportConfiguration where
  toJSON WaitingReportConfiguration {..} =
    object
      [ "threshold" .= waitingReportConfThreshold
      ]

instance FromJSON WaitingReportConfiguration where
  parseJSON = viaYamlSchema

instance YamlSchema WaitingReportConfiguration where
  yamlSchema =
    objectParser "WaitingReportConfiguration" $
      WaitingReportConfiguration
        <$> optionalField "threshold" "waiting report threshold to consider waiting entries 'overdue'"

backToWaitingReportConfiguration :: WaitingReportConfig -> WaitingReportConfiguration
backToWaitingReportConfiguration WaitingReportConfig {..} =
  WaitingReportConfiguration
    { waitingReportConfThreshold =
        if waitingReportConfigThreshold == defaultWaitingThreshold
          then Nothing
          else Just defaultWaitingThreshold
    }

data StuckReportConfiguration = StuckReportConfiguration
  { stuckReportConfThreshold :: !(Maybe Word)
  }
  deriving (Show, Eq, Generic)

instance Validity StuckReportConfiguration

instance ToJSON StuckReportConfiguration where
  toJSON StuckReportConfiguration {..} =
    object
      [ "threshold" .= stuckReportConfThreshold
      ]

instance FromJSON StuckReportConfiguration where
  parseJSON = viaYamlSchema

instance YamlSchema StuckReportConfiguration where
  yamlSchema =
    objectParser "StuckReportConfiguration" $
      StuckReportConfiguration
        <$> optionalField "threshold" "stuck report threshold to consider stuck projects 'overdue'"

backToStuckReportConfiguration :: StuckReportConfig -> StuckReportConfiguration
backToStuckReportConfiguration StuckReportConfig {..} =
  StuckReportConfiguration
    { stuckReportConfThreshold =
        if stuckReportConfigThreshold == defaultStuckThreshold
          then Nothing
          else Just defaultStuckThreshold
    }

data WorkReportConfiguration = WorkReportConfiguration
  { workReportConfBaseFilter :: !(Maybe EntryFilterRel),
    workReportConfChecks :: !(Maybe (Set EntryFilterRel)),
    workReportConfContexts :: !(Maybe (Map ContextName EntryFilterRel)),
    workReportConfTimeFilterProperty :: Maybe PropertyName,
    workReportConfProjection :: Maybe (NonEmpty Projection),
    workReportConfSorter :: Maybe Sorter
  }
  deriving (Show, Eq, Generic)

instance Validity WorkReportConfiguration

instance ToJSON WorkReportConfiguration where
  toJSON WorkReportConfiguration {..} =
    object
      [ "base-filter" .= workReportConfBaseFilter,
        "checks" .= workReportConfChecks,
        "contexts" .= workReportConfContexts,
        "time-filter" .= workReportConfTimeFilterProperty,
        "columns" .= workReportConfProjection,
        "sorter" .= workReportConfSorter
      ]

instance FromJSON WorkReportConfiguration where
  parseJSON = viaYamlSchema

instance YamlSchema WorkReportConfiguration where
  yamlSchema =
    objectParser "WorkReportConfiguration" $
      WorkReportConfiguration
        <$> optionalField "base-filter" "The base work filter"
        <*> optionalField "checks" "Checks for the work report"
        <*> optionalField "contexts" "Contexts for the work report"
        <*> optionalField "time-filter" "The property to use to filter by time"
        <*> optionalField "columns" "The columns in the report"
        <*> optionalField "sorter" "The sorter to use to sort the rows"

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
