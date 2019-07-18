{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Report.Config
  ( SmosReportConfig(..)
  , defaultReportConfig, defaultWorkflowDirSpec
  , WorkflowDirSpec(..)
  , resolveWorkflowDir
  ) where

import GHC.Generics (Generic)

import Path
import Path.IO

data SmosReportConfig =
  SmosReportConfig
    { smosReportConfigAgendaFileSpec :: !WorkflowDirSpec
    }
  deriving (Show, Eq, Generic)

defaultReportConfig :: SmosReportConfig
defaultReportConfig = SmosReportConfig {smosReportConfigAgendaFileSpec = defaultWorkflowDirSpec}

defaultWorkflowDirSpec :: WorkflowDirSpec
defaultWorkflowDirSpec = DirInHome [reldir|workflow|]

data WorkflowDirSpec
  = DirInHome (Path Rel Dir)
  | DirAbsolute (Path Abs Dir)
  deriving (Show, Eq, Generic)

resolveWorkflowDir :: WorkflowDirSpec -> IO (Path Abs Dir)
resolveWorkflowDir afs =
  case afs of
    DirInHome rp -> getHomeDir >>= (`resolveDir` fromRelDir rp)
    DirAbsolute ad -> pure ad
