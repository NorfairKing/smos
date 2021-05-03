module Smos.Query.Streaming where

import Conduit
import Control.Monad.Reader
import Path
import Smos.Data
import Smos.Query.Env
import Smos.Report.Archive
import Smos.Report.Config
import Smos.Report.Streaming

streamSmosProjectsQ :: ConduitT i (Path Rel File, SmosFile) Q ()
streamSmosProjectsQ = do
  dc <- lift $ asks envDirectoryConfig
  streamSmosProjectsFiles dc .| streamParseSmosProjects

streamSmosFiles :: HideArchive -> ConduitT i (Path Rel File) Q ()
streamSmosFiles ha = do
  dc <- lift $ asks envDirectoryConfig
  streamSmosFilesFromWorkflowRel ha dc

streamAllSmosFiles :: ConduitT i (Path Rel File) Q ()
streamAllSmosFiles = do
  dc <- lift $ asks envDirectoryConfig
  streamSmosFilesFromWorkflowRel Don'tHideArchive dc

streamParseSmosProjects :: ConduitT (Path Rel File) (Path Rel File, SmosFile) Q ()
streamParseSmosProjects = do
  dc <- lift $ asks envDirectoryConfig
  pd <- liftIO $ resolveDirProjectsDir dc
  parseSmosFilesRel pd .| shouldPrintC

streamParseSmosFiles :: ConduitT (Path Rel File) (Path Rel File, SmosFile) Q ()
streamParseSmosFiles = do
  dc <- lift $ asks envDirectoryConfig
  wd <- liftIO $ resolveDirWorkflowDir dc
  parseSmosFilesRel wd .| shouldPrintC

shouldPrintC :: ConduitT (a, Either ParseSmosFileException b) (a, b) Q ()
shouldPrintC = do
  sp <- lift getShouldPrint
  printShouldPrint sp
