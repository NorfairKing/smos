{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.OptParse.Types where

import Data.Map (Map)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Validity
import Data.Yaml as Yaml
import GHC.Generics (Generic)
import Path
import Smos.Report.Config
import Smos.Report.Filter
import YamlParse.Applicative as YamlParse

data Flags
  = Flags
      { flagDirectoryFlags :: DirectoryFlags
      }
  deriving (Show, Eq, Generic)

data FlagsWithConfigFile a
  = FlagsWithConfigFile
      { flagWithConfigFile :: Maybe FilePath,
        flagWithRestFlags :: a
      }
  deriving (Show, Eq, Generic)

data DirectoryFlags
  = DirectoryFlags
      { dirFlagWorkflowDir :: Maybe FilePath,
        dirFlagArchiveDir :: Maybe FilePath,
        dirFlagProjectsDir :: Maybe FilePath,
        dirFlagArchivedProjectsDir :: Maybe FilePath
      }
  deriving (Show, Eq, Generic)

data Environment
  = Environment
      { envDirectoryEnvironment :: DirectoryEnvironment
      }
  deriving (Show, Eq, Generic)

data EnvWithConfigFile a
  = EnvWithConfigFile
      { envWithConfigFile :: Maybe FilePath,
        envWithRestEnv :: a
      }
  deriving (Show, Eq, Generic)

data DirectoryEnvironment
  = DirectoryEnvironment
      { dirEnvWorkflowDir :: Maybe FilePath,
        dirEnvArchiveDir :: Maybe FilePath,
        dirEnvProjectsDir :: Maybe FilePath,
        dirEnvArchivedProjectsDir :: Maybe FilePath
      }
  deriving (Show, Eq, Generic)

data Configuration
  = Configuration
      { confDirectoryConf :: !DirectoryConfiguration,
        confWorkReportConf :: !(Maybe WorkReportConfiguration)
      }
  deriving (Show, Eq, Generic)

instance Validity Configuration

instance ToJSON Configuration where
  toJSON Configuration {..} =
    object $
      directoryConfigurationToObject confDirectoryConf
        ++ [ "work" .= confWorkReportConf
           ]

instance FromJSON Configuration where
  parseJSON = viaYamlSchema

instance YamlSchema Configuration where
  yamlSchema =
    objectParser "Configuration" $
      Configuration
        <$> directoryConfigurationObjectParser
        <*> optionalField "work" "The work report configuration"

backToConfiguration :: SmosReportConfig -> Configuration
backToConfiguration SmosReportConfig {..} =
  Configuration
    { confDirectoryConf = backToDirectoryConfiguration smosReportConfigDirectoryConfig,
      confWorkReportConf = if smosReportConfigWorkConfig == defaultWorkReportConfig then Nothing else Just $ backToWorkReportConfiguration smosReportConfigWorkConfig
    }

data DirectoryConfiguration
  = DirectoryConfiguration
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
          else Just
            $ T.pack
            $ case directoryConfigWorkflowFileSpec of
              DirInHome rd -> "~/" <> fromRelDir rd
              DirAbsolute ad -> fromAbsDir ad,
      directoryConfArchiveDir =
        if directoryConfigArchiveFileSpec == defaultArchiveDirSpec
          then Nothing
          else Just
            $ T.pack
            $ case directoryConfigArchiveFileSpec of
              ArchiveInWorkflow ard -> fromRelDir ard
              ArchiveInHome ard -> "~/" <> fromRelDir ard
              ArchiveAbsolute aad -> fromAbsDir aad,
      directoryConfProjectsDir =
        if directoryConfigProjectsFileSpec == defaultProjectsDirSpec
          then Nothing
          else Just
            $ T.pack
            $ case directoryConfigProjectsFileSpec of
              ProjectsInWorkflow ard -> fromRelDir ard
              ProjectsInHome ard -> "~/" <> fromRelDir ard
              ProjectsAbsolute aad -> fromAbsDir aad,
      directoryConfArchivedProjectsDir =
        if directoryConfigArchivedProjectsFileSpec == defaultArchivedProjectsDirSpec
          then Nothing
          else Just
            $ T.pack
            $ case directoryConfigArchivedProjectsFileSpec of
              ArchivedProjectsInArchive ard -> fromRelDir ard
              ArchivedProjectsInHome ard -> "~/" <> fromRelDir ard
              ArchivedProjectsAbsolute aad -> fromAbsDir aad
    }

data WorkReportConfiguration
  = WorkReportConfiguration
      { workReportConfBaseFilter :: !(Maybe EntryFilterRel),
        workReportConfContexts :: !(Maybe (Map ContextName EntryFilterRel))
      }
  deriving (Show, Eq, Generic)

instance Validity WorkReportConfiguration

instance ToJSON WorkReportConfiguration where
  toJSON WorkReportConfiguration {..} =
    object
      [ "base-filter" .= workReportConfBaseFilter,
        "contexts" .= workReportConfContexts
      ]

instance FromJSON WorkReportConfiguration where
  parseJSON = viaYamlSchema

instance YamlSchema WorkReportConfiguration where
  yamlSchema =
    objectParser "WorkReportConfiguration" $
      WorkReportConfiguration
        <$> optionalField "base-filter" "The base work filter"
        <*> optionalField "contexts" "Contexts for the work report"

backToWorkReportConfiguration :: WorkReportConfig -> WorkReportConfiguration
backToWorkReportConfiguration WorkReportConfig {..} =
  WorkReportConfiguration
    { workReportConfBaseFilter =
        if workReportConfigBaseFilter == Just defaultWorkBaseFilter
          then Nothing
          else Just defaultWorkBaseFilter,
      workReportConfContexts = Just workReportConfigContexts
    }
