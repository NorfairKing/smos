{-# LANGUAGE DeriveGeneric #-}

module Smos.Query.Config
  ( SmosQueryConfig(..)
  , HideArchive(..)
  , Q
  , askWorkflowDir
  , askArchiveDir
  , module Smos.Report.Config
  , module Control.Monad.IO.Class
  , module Control.Monad.Reader
  ) where

import GHC.Generics (Generic)

import Path

import Control.Monad.IO.Class
import Control.Monad.Reader

import Smos.Report.Archive
import Smos.Report.Config

data SmosQueryConfig =
  SmosQueryConfig
    { smosQueryConfigReportConfig :: SmosReportConfig
    , smosQueryConfigHideArchive :: HideArchive
    }
  deriving (Show, Eq, Generic)

type Q = ReaderT SmosQueryConfig IO

askWorkflowDir :: Q (Path Abs Dir)
askWorkflowDir = do
  func <- asks (resolveReportWorkflowDir . smosQueryConfigReportConfig)
  liftIO func

askArchiveDir :: Q (Path Abs Dir)
askArchiveDir = do
  func <- asks (resolveReportArchiveDir . smosQueryConfigReportConfig)
  liftIO func
