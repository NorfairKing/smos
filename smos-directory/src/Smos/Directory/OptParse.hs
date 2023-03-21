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
import qualified Data.Text as T
import qualified Env
import Options.Applicative
import Path.IO
import Smos.Directory.Config
import Smos.Directory.OptParse.Types

combineToDirectorySettings :: DirectorySettings -> DirectoryFlags -> DirectoryEnvironment -> Maybe DirectorySettingsuration -> IO DirectorySettings
combineToDirectorySettings dc DirectoryFlags {..} DirectoryEnvironment {..} mc = do
  wfs <-
    case msum [dirFlagWorkflowDir, dirEnvWorkflowDir, mc >>= (fmap T.unpack . directoryConfWorkflowDir)] of
      Nothing -> pure $ directoryConfigWorkflowFileSpec dc
      Just wd -> do
        ad <- resolveDir' wd
        pure $ AbsoluteWorkflow ad
  afs <-
    case msum [dirFlagArchiveDir, dirEnvArchiveDir, mc >>= (fmap T.unpack . directoryConfArchiveDir)] of
      Nothing -> pure $ directoryConfigArchiveFileSpec dc
      Just wd -> do
        ad <- resolveDir' wd
        pure $ ArchiveAbsolute ad
  pfs <-
    case msum [dirFlagProjectsDir, dirEnvProjectsDir, mc >>= (fmap T.unpack . directoryConfProjectsDir)] of
      Nothing -> pure $ directoryConfigProjectsFileSpec dc
      Just wd -> do
        ad <- resolveDir' wd
        pure $ ProjectsAbsolute ad
  apfs <-
    case msum
      [ dirFlagArchivedProjectsDir,
        dirEnvArchivedProjectsDir,
        mc >>= (fmap T.unpack . directoryConfArchivedProjectsDir)
      ] of
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
    <$> optional (Env.var Env.str "WORKFLOW_DIR" (Env.help "Workflow directory"))
    <*> optional (Env.var Env.str "ARCHIVE_DIR" (Env.help "Archive directory"))
    <*> optional (Env.var Env.str "PROJECTS_DIR" (Env.help "Projects directory"))
    <*> optional (Env.var Env.str "ARCHIVED_PROJECTS_DIR" (Env.help "Archived projects directory"))
