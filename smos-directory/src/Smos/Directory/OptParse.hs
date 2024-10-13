{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Smos.Directory.OptParse where

import OptEnvConf
import Path

data DirectorySettings = DirectorySettings
  { directoryConfigWorkflowFileSpec :: !WorkflowDirSpec,
    directoryConfigArchiveFileSpec :: !ArchiveDirSpec,
    directoryConfigProjectsFileSpec :: !ProjectsDirSpec,
    directoryConfigArchivedProjectsFileSpec :: !ArchivedProjectsDirSpec
  }

instance HasParser DirectorySettings where
  settingsParser = parseDirectorySettings

{-# ANN parseDirectorySettings ("NOCOVER" :: String) #-}
parseDirectorySettings :: Parser DirectorySettings
parseDirectorySettings = do
  directoryConfigWorkflowFileSpec <- settingsParser
  directoryConfigArchiveFileSpec <- settingsParser
  directoryConfigProjectsFileSpec <- settingsParser
  directoryConfigArchivedProjectsFileSpec <- settingsParser
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

instance HasParser WorkflowDirSpec where
  settingsParser =
    choice
      [ AbsoluteWorkflow
          <$> directoryPathSetting
            [ help "The workflow directory",
              name "workflow-dir"
            ],
        pure defaultWorkflowDirSpec
      ]

defaultWorkflowDirSpec :: WorkflowDirSpec
defaultWorkflowDirSpec = WorkflowInHome [reldir|workflow|]

data ArchiveDirSpec
  = ArchiveInWorkflow (Path Rel Dir)
  | ArchiveInHome (Path Rel Dir)
  | ArchiveAbsolute (Path Abs Dir)

instance HasParser ArchiveDirSpec where
  settingsParser =
    choice
      [ ArchiveAbsolute
          <$> directoryPathSetting
            [ help "The archive directory",
              name "archive-dir"
            ],
        pure defaultArchiveDirSpec
      ]

defaultArchiveDirSpec :: ArchiveDirSpec
defaultArchiveDirSpec = ArchiveInWorkflow [reldir|archive|]

data ProjectsDirSpec
  = ProjectsInWorkflow (Path Rel Dir)
  | ProjectsInHome (Path Rel Dir)
  | ProjectsAbsolute (Path Abs Dir)

instance HasParser ProjectsDirSpec where
  settingsParser =
    choice
      [ ProjectsAbsolute
          <$> directoryPathSetting
            [ help "The projects directory",
              name "projects-dir"
            ],
        pure defaultProjectsDirSpec
      ]

defaultProjectsDirSpec :: ProjectsDirSpec
defaultProjectsDirSpec = ProjectsInWorkflow [reldir|projects|]

data ArchivedProjectsDirSpec
  = ArchivedProjectsInArchive (Path Rel Dir)
  | ArchivedProjectsInHome (Path Rel Dir)
  | ArchivedProjectsAbsolute (Path Abs Dir)

instance HasParser ArchivedProjectsDirSpec where
  settingsParser =
    choice
      [ ArchivedProjectsAbsolute
          <$> directoryPathSetting
            [ help "The archived projects directory",
              name "archived-projects-dir"
            ],
        pure defaultArchivedProjectsDirSpec
      ]

defaultArchivedProjectsDirSpec :: ArchivedProjectsDirSpec
defaultArchivedProjectsDirSpec = ArchivedProjectsInArchive [reldir|projects|]
