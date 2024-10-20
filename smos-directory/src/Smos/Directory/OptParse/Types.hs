{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Directory.OptParse.Types where

import Autodocodec
import Data.Validity
import Data.Validity.Path ()
import GHC.Generics (Generic)
import Path

data DirectoryFlags = DirectoryFlags
  { dirFlagWorkflowDir :: Maybe FilePath,
    dirFlagArchiveDir :: Maybe FilePath,
    dirFlagProjectsDir :: Maybe FilePath,
    dirFlagArchivedProjectsDir :: Maybe FilePath
  }

data DirectoryEnvironment = DirectoryEnvironment
  { dirEnvWorkflowDir :: Maybe FilePath,
    dirEnvArchiveDir :: Maybe FilePath,
    dirEnvProjectsDir :: Maybe FilePath,
    dirEnvArchivedProjectsDir :: Maybe FilePath
  }

data DirectoryConfiguration = DirectoryConfiguration
  { directoryConfWorkflowDir :: !(Maybe FilePath),
    directoryConfArchiveDir :: !(Maybe FilePath),
    directoryConfProjectsDir :: !(Maybe FilePath),
    directoryConfArchivedProjectsDir :: !(Maybe FilePath)
  }
  deriving stock (Show, Eq, Generic)

instance Validity DirectoryConfiguration

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

backToDirectoryConfiguration :: DirectorySettings -> DirectoryConfiguration
backToDirectoryConfiguration DirectorySettings {..} =
  DirectoryConfiguration
    { directoryConfWorkflowDir =
        if directoryConfigWorkflowFileSpec == defaultWorkflowDirSpec
          then Nothing
          else Just $
            case directoryConfigWorkflowFileSpec of
              WorkflowInHome rd -> "~/" <> fromRelDir rd
              AbsoluteWorkflow ad -> fromAbsDir ad,
      directoryConfArchiveDir =
        if directoryConfigArchiveFileSpec == defaultArchiveDirSpec
          then Nothing
          else Just $
            case directoryConfigArchiveFileSpec of
              ArchiveInWorkflow ard -> fromRelDir ard
              ArchiveInHome ard -> "~/" <> fromRelDir ard
              ArchiveAbsolute aad -> fromAbsDir aad,
      directoryConfProjectsDir =
        if directoryConfigProjectsFileSpec == defaultProjectsDirSpec
          then Nothing
          else Just $
            case directoryConfigProjectsFileSpec of
              ProjectsInWorkflow ard -> fromRelDir ard
              ProjectsInHome ard -> "~/" <> fromRelDir ard
              ProjectsAbsolute aad -> fromAbsDir aad,
      directoryConfArchivedProjectsDir =
        if directoryConfigArchivedProjectsFileSpec == defaultArchivedProjectsDirSpec
          then Nothing
          else Just $
            case directoryConfigArchivedProjectsFileSpec of
              ArchivedProjectsInArchive ard -> fromRelDir ard
              ArchivedProjectsInHome ard -> "~/" <> fromRelDir ard
              ArchivedProjectsAbsolute aad -> fromAbsDir aad
    }

data DirectorySettings = DirectorySettings
  { directoryConfigWorkflowFileSpec :: !WorkflowDirSpec,
    directoryConfigArchiveFileSpec :: !ArchiveDirSpec,
    directoryConfigProjectsFileSpec :: !ProjectsDirSpec,
    directoryConfigArchivedProjectsFileSpec :: !ArchivedProjectsDirSpec
  }

defaultDirectorySettings :: DirectorySettings
defaultDirectorySettings =
  DirectorySettings
    { directoryConfigWorkflowFileSpec = defaultWorkflowDirSpec,
      directoryConfigArchiveFileSpec = defaultArchiveDirSpec,
      directoryConfigProjectsFileSpec = defaultProjectsDirSpec,
      directoryConfigArchivedProjectsFileSpec = defaultArchivedProjectsDirSpec
    }

data WorkflowDirSpec
  = WorkflowInHome (Path Rel Dir)
  | AbsoluteWorkflow (Path Abs Dir)
  deriving (Eq)

defaultWorkflowDirSpec :: WorkflowDirSpec
defaultWorkflowDirSpec = WorkflowInHome [reldir|workflow|]

data ArchiveDirSpec
  = ArchiveInWorkflow (Path Rel Dir)
  | ArchiveInHome (Path Rel Dir)
  | ArchiveAbsolute (Path Abs Dir)
  deriving (Eq)

defaultArchiveDirSpec :: ArchiveDirSpec
defaultArchiveDirSpec = ArchiveInWorkflow [reldir|archive|]

data ProjectsDirSpec
  = ProjectsInWorkflow (Path Rel Dir)
  | ProjectsInHome (Path Rel Dir)
  | ProjectsAbsolute (Path Abs Dir)
  deriving (Eq)

defaultProjectsDirSpec :: ProjectsDirSpec
defaultProjectsDirSpec = ProjectsInWorkflow [reldir|projects|]

data ArchivedProjectsDirSpec
  = ArchivedProjectsInArchive (Path Rel Dir)
  | ArchivedProjectsInHome (Path Rel Dir)
  | ArchivedProjectsAbsolute (Path Abs Dir)
  deriving (Eq)

defaultArchivedProjectsDirSpec :: ArchivedProjectsDirSpec
defaultArchivedProjectsDirSpec = ArchivedProjectsInArchive [reldir|projects|]
