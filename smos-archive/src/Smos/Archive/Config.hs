{-# LANGUAGE DeriveGeneric #-}

module Smos.Archive.Config
  ( SmosArchiveConfig(..)
  , Q
  , askWorkflowDir
  , askArchiveDir
  , module Smos.Report.Config
  , module Control.Monad.IO.Class
  , module Control.Monad.Reader
  ) where

import GHC.Generics

import Path

import Control.Monad.IO.Class
import Control.Monad.Reader

import Smos.Report.Config

newtype SmosArchiveConfig =
  SmosArchiveConfig
    { smosArchiveConfigReportConfig :: SmosReportConfig
    }
  deriving (Generic)

type Q = ReaderT SmosArchiveConfig IO

askWorkflowDir :: Q (Path Abs Dir)
askWorkflowDir = do
  rc <- asks smosArchiveConfigReportConfig
  liftIO $ resolveReportWorkflowDir rc

askArchiveDir :: Q (Path Abs Dir)
askArchiveDir = do
  rc <- asks smosArchiveConfigReportConfig
  liftIO $ resolveReportArchiveDir rc
