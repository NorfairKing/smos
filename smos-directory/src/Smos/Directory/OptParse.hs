{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Smos.Directory.OptParse where

import qualified OptEnvConf
import Path

data DirectorySettings = DirectorySettings
  { directoryConfigWorkflowFileSpec :: !WorkflowDirSpec,
    directoryConfigArchiveFileSpec :: !ArchiveDirSpec,
    directoryConfigProjectsFileSpec :: !ProjectsDirSpec,
    directoryConfigArchivedProjectsFileSpec :: !ArchivedProjectsDirSpec
  }

instance OptEnvConf.HasParser DirectorySettings where
  settingsParser = parseDirectorySettings

{-# ANN parseDirectorySettings ("NOCOVER" :: String) #-}
parseDirectorySettings :: OptEnvConf.Parser DirectorySettings
parseDirectorySettings = do
  directoryConfigWorkflowFileSpec <- OptEnvConf.settingsParser
  directoryConfigArchiveFileSpec <- OptEnvConf.settingsParser
  directoryConfigProjectsFileSpec <- OptEnvConf.settingsParser
  directoryConfigArchivedProjectsFileSpec <- OptEnvConf.settingsParser
  pure DirectorySettings {..}

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

instance OptEnvConf.HasParser WorkflowDirSpec where
  settingsParser =
    OptEnvConf.choice
      [ AbsoluteWorkflow
          <$> OptEnvConf.directoryPathSetting
            [ OptEnvConf.help "The workflow directory",
              OptEnvConf.name "workflow-dir"
            ],
        pure defaultWorkflowDirSpec
      ]

defaultWorkflowDirSpec :: WorkflowDirSpec
defaultWorkflowDirSpec = WorkflowInHome [reldir|workflow|]

data ArchiveDirSpec
  = ArchiveInWorkflow (Path Rel Dir)
  | ArchiveInHome (Path Rel Dir)
  | ArchiveAbsolute (Path Abs Dir)

instance OptEnvConf.HasParser ArchiveDirSpec where
  settingsParser =
    OptEnvConf.choice
      [ ArchiveAbsolute
          <$> OptEnvConf.directoryPathSetting
            [ OptEnvConf.help "The archive directory",
              OptEnvConf.name "archive-dir"
            ],
        pure defaultArchiveDirSpec
      ]

defaultArchiveDirSpec :: ArchiveDirSpec
defaultArchiveDirSpec = ArchiveInWorkflow [reldir|archive|]

data ProjectsDirSpec
  = ProjectsInWorkflow (Path Rel Dir)
  | ProjectsInHome (Path Rel Dir)
  | ProjectsAbsolute (Path Abs Dir)

instance OptEnvConf.HasParser ProjectsDirSpec where
  settingsParser =
    OptEnvConf.choice
      [ ProjectsAbsolute
          <$> OptEnvConf.directoryPathSetting
            [ OptEnvConf.help "The projects directory",
              OptEnvConf.name "projects-dir"
            ],
        pure defaultProjectsDirSpec
      ]

defaultProjectsDirSpec :: ProjectsDirSpec
defaultProjectsDirSpec = ProjectsInWorkflow [reldir|projects|]

data ArchivedProjectsDirSpec
  = ArchivedProjectsInArchive (Path Rel Dir)
  | ArchivedProjectsInHome (Path Rel Dir)
  | ArchivedProjectsAbsolute (Path Abs Dir)

instance OptEnvConf.HasParser ArchivedProjectsDirSpec where
  settingsParser =
    OptEnvConf.choice
      [ ArchivedProjectsAbsolute
          <$> OptEnvConf.directoryPathSetting
            [ OptEnvConf.help "The archived projects directory",
              OptEnvConf.name "archived-projects-dir"
            ],
        pure defaultArchivedProjectsDirSpec
      ]

defaultArchivedProjectsDirSpec :: ArchivedProjectsDirSpec
defaultArchivedProjectsDirSpec = ArchivedProjectsInArchive [reldir|projects|]
