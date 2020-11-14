module Smos.Query.Streaming where

import Conduit
import Path
import Smos.Data
import Smos.Query.Config
import Smos.Report.ShouldPrint
import Smos.Report.Streaming

streamSmosProjects :: ConduitT i (Path Rel File) Q ()
streamSmosProjects = do
  src <- lift $ asks $ smosReportConfigDirectoryConfig . smosQueryConfigReportConfig
  streamSmosProjectsFiles src

streamSmosFiles :: HideArchive -> ConduitT i (Path Rel File) Q ()
streamSmosFiles ha = do
  src <- lift $ asks $ smosReportConfigDirectoryConfig . smosQueryConfigReportConfig
  streamSmosFilesFromWorkflowRel ha src

streamAllSmosFiles :: ConduitT i (Path Rel File) Q ()
streamAllSmosFiles = do
  src <- lift $ asks $ smosReportConfigDirectoryConfig . smosQueryConfigReportConfig
  streamSmosFilesFromWorkflowRel Don'tHideArchive src

streamParseSmosProjects :: ConduitT (Path Rel File) (Path Rel File, SmosFile) Q ()
streamParseSmosProjects = do
  src <- lift $ asks $ smosReportConfigDirectoryConfig . smosQueryConfigReportConfig
  pd <- liftIO $ resolveDirProjectsDir src
  parseSmosFilesRel pd .| printShouldPrint PrintWarning

streamParseSmosFiles :: ConduitT (Path Rel File) (Path Rel File, SmosFile) Q ()
streamParseSmosFiles = do
  src <- lift $ asks $ smosReportConfigDirectoryConfig . smosQueryConfigReportConfig
  wd <- liftIO $ resolveDirWorkflowDir src
  parseSmosFilesRel wd .| printShouldPrint PrintWarning
