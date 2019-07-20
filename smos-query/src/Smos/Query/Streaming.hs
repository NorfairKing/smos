module Smos.Query.Streaming where

import Conduit
import Data.Conduit.Combinators as Conduit
import Path

import Smos.Report.Path
import Smos.Report.Streaming

import Smos.Query.Config

streamSmosFiles :: ConduitT i RootedPath Q ()
streamSmosFiles = do
  wd <- lift askWorkflowDir
  ads <- lift $ asks $ smosReportConfigArchiveFileSpec . smosQueryConfigReportConfig
  ha <- lift $ asks smosQueryConfigHideArchive
  streamSmosFilesFromWorkflow ha src
