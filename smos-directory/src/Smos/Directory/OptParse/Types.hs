{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Directory.OptParse.Types where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Text ()
import GHC.Generics (Generic)
import Path
import Smos.Directory.Config

data DirectoryFlags = DirectoryFlags
  { dirFlagWorkflowDir :: Maybe FilePath,
    dirFlagArchiveDir :: Maybe FilePath,
    dirFlagProjectsDir :: Maybe FilePath,
    dirFlagArchivedProjectsDir :: Maybe FilePath
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

data DirectorySettingsuration = DirectorySettingsuration
  { directoryConfWorkflowDir :: !(Maybe Text),
    directoryConfArchiveDir :: !(Maybe Text),
    directoryConfProjectsDir :: !(Maybe Text),
    directoryConfArchivedProjectsDir :: !(Maybe Text)
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via (Autodocodec DirectorySettingsuration)

instance Validity DirectorySettingsuration

instance HasCodec DirectorySettingsuration where
  codec = object "DirectorySettingsuration" objectCodec

instance HasObjectCodec DirectorySettingsuration where
  objectCodec =
    DirectorySettingsuration
      <$> optionalFieldOrNull "workflow-dir" "The workflow directory" .= directoryConfWorkflowDir
      <*> optionalFieldOrNull "archive-dir" "The archive directory" .= directoryConfArchiveDir
      <*> optionalFieldOrNull "projects-dir" "The projects directory" .= directoryConfProjectsDir
      <*> optionalFieldOrNull "archived-projects-dir" "The archived projects directory" .= directoryConfArchivedProjectsDir

defaultDirectorySettingsuration :: DirectorySettingsuration
defaultDirectorySettingsuration =
  DirectorySettingsuration
    { directoryConfWorkflowDir = Nothing,
      directoryConfArchiveDir = Nothing,
      directoryConfProjectsDir = Nothing,
      directoryConfArchivedProjectsDir = Nothing
    }

backToDirectorySettingsuration :: DirectorySettings -> DirectorySettingsuration
backToDirectorySettingsuration DirectorySettings {..} =
  DirectorySettingsuration
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
