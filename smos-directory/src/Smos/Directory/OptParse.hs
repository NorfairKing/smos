{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Smos.Directory.OptParse
  ( module Smos.Directory.OptParse,
    module Smos.Directory.OptParse.Types,
  )
where

import Control.Applicative
import Control.Monad
import qualified Env
import Options.Applicative
import Path.IO
import Smos.Directory.OptParse.Types

combineToDirectorySettings :: DirectorySettings -> DirectoryFlags -> DirectoryEnvironment -> Maybe DirectoryConfiguration -> IO DirectorySettings
combineToDirectorySettings dc DirectoryFlags {..} DirectoryEnvironment {..} mc = do
  wfs <-
    combineToWorkflowDirSpec
      (directoryConfigWorkflowFileSpec dc)
      dirFlagWorkflowDir
      dirEnvWorkflowDir
      (mc >>= directoryConfWorkflowDir)
  afs <-
    case msum [dirFlagArchiveDir, dirEnvArchiveDir, mc >>= directoryConfArchiveDir] of
      Nothing -> pure $ directoryConfigArchiveFileSpec dc
      Just wd -> do
        ad <- resolveDir' wd
        pure $ ArchiveAbsolute ad
  pfs <-
    case msum [dirFlagProjectsDir, dirEnvProjectsDir, mc >>= directoryConfProjectsDir] of
      Nothing -> pure $ directoryConfigProjectsFileSpec dc
      Just wd -> do
        ad <- resolveDir' wd
        pure $ ProjectsAbsolute ad
  apfs <-
    case msum [dirFlagArchivedProjectsDir, dirEnvArchivedProjectsDir, mc >>= directoryConfArchivedProjectsDir] of
      Nothing -> pure $ directoryConfigArchivedProjectsFileSpec dc
      Just wd -> do
        ad <- resolveDir' wd
        pure $ ArchivedProjectsAbsolute ad
  pure $
    dc
      { directoryConfigWorkflowFileSpec = wfs,
        directoryConfigArchiveFileSpec = afs,
        directoryConfigProjectsFileSpec = pfs,
        directoryConfigArchivedProjectsFileSpec = apfs
      }

combineToWorkflowDirSpec ::
  WorkflowDirSpec ->
  Maybe FilePath ->
  Maybe FilePath ->
  Maybe FilePath ->
  IO WorkflowDirSpec
combineToWorkflowDirSpec defaultDirSpec flagWorkflowDir envWorkflowDir confWorkflowDir =
  case flagWorkflowDir <|> envWorkflowDir <|> confWorkflowDir of
    Nothing -> pure defaultDirSpec
    Just wd -> AbsoluteWorkflow <$> resolveDir' wd

parseDirectoryFlags :: Parser DirectoryFlags
parseDirectoryFlags =
  DirectoryFlags
    <$> parseWorkflowDirFlag
    <*> parseArchiveDirFlag
    <*> parseProjectsDirFlag
    <*> parseArchivedProjectsDirFlag

parseWorkflowDirFlag :: Parser (Maybe FilePath)
parseWorkflowDirFlag =
  optional
    ( strOption
        ( mconcat
            [ metavar "DIRECTORY_PATH",
              help "The workflow directory to use",
              long "workflow-dir",
              completer $ bashCompleter "directory"
            ]
        )
    )

parseArchiveDirFlag :: Parser (Maybe FilePath)
parseArchiveDirFlag =
  optional
    ( strOption
        ( mconcat
            [ metavar "DIRECTORY_PATH",
              help "The archive directory to use",
              long "archive-dir",
              completer $ bashCompleter "directory"
            ]
        )
    )

parseProjectsDirFlag :: Parser (Maybe FilePath)
parseProjectsDirFlag =
  optional
    ( strOption
        ( mconcat
            [ metavar "DIRECTORY_PATH",
              help "The projects directory to use",
              long "projects-dir",
              completer $ bashCompleter "directory"
            ]
        )
    )

parseArchivedProjectsDirFlag :: Parser (Maybe FilePath)
parseArchivedProjectsDirFlag =
  optional
    ( strOption
        ( mconcat
            [ metavar "DIRECTORY_PATHPATH",
              help "The archived projects directory to use",
              long "archived-projects-dir",
              completer $ bashCompleter "directory"
            ]
        )
    )

directoryEnvironmentParser :: Env.Parser Env.Error DirectoryEnvironment
directoryEnvironmentParser =
  DirectoryEnvironment
    <$> workflowDirEnvParser
    <*> optional (Env.var Env.str "ARCHIVE_DIR" (Env.help "Archive directory"))
    <*> optional (Env.var Env.str "PROJECTS_DIR" (Env.help "Projects directory"))
    <*> optional (Env.var Env.str "ARCHIVED_PROJECTS_DIR" (Env.help "Archived projects directory"))

workflowDirEnvParser :: Env.Parser Env.Error (Maybe FilePath)
workflowDirEnvParser = optional (Env.var Env.str "WORKFLOW_DIR" (Env.help "Workflow directory"))
