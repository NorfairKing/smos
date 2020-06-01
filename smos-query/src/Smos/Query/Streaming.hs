module Smos.Query.Streaming where

import Conduit
import Smos.Query.Config
import Smos.Report.Path
import Smos.Report.Streaming

streamSmosProjects :: ConduitT i RootedPath Q ()
streamSmosProjects = do
  src <- lift $ asks $ smosReportConfigDirectoryConfig . smosQueryConfigReportConfig
  streamSmosProjectsFiles src

streamSmosFiles :: HideArchive -> ConduitT i RootedPath Q ()
streamSmosFiles ha = do
  src <- lift $ asks $ smosReportConfigDirectoryConfig . smosQueryConfigReportConfig
  streamSmosFilesFromWorkflow ha src

streamAllSmosFiles :: ConduitT i RootedPath Q ()
streamAllSmosFiles = do
  src <- lift $ asks $ smosReportConfigDirectoryConfig . smosQueryConfigReportConfig
  streamSmosFilesFromWorkflow Don'tHideArchive src
