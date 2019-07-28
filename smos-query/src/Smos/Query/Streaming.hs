module Smos.Query.Streaming where

import Conduit

import Smos.Report.Path
import Smos.Report.Streaming

import Smos.Query.Config

streamSmosProjects :: ConduitT i RootedPath Q ()
streamSmosProjects = do
  src <- lift $ asks smosQueryConfigReportConfig
  streamSmosProjectsFiles src

streamSmosFiles :: ConduitT i RootedPath Q ()
streamSmosFiles = do
  src <- lift $ asks smosQueryConfigReportConfig
  ha <- lift $ asks smosQueryConfigHideArchive
  streamSmosFilesFromWorkflow ha src

streamAllSmosFiles :: ConduitT i RootedPath Q ()
streamAllSmosFiles = do
  src <- lift $ asks smosQueryConfigReportConfig
  streamSmosFilesFromWorkflow Don'tHideArchive src
