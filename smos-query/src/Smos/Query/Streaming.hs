module Smos.Query.Streaming where

import Conduit

import Smos.Report.Path
import Smos.Report.Streaming

import Smos.Query.Config

streamSmosFiles :: ConduitT i RootedPath Q ()
streamSmosFiles = do
  src <- lift $ asks smosQueryConfigReportConfig
  ha <- lift $ asks smosQueryConfigHideArchive
  streamSmosFilesFromWorkflow ha src
