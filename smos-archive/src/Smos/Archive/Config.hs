{-# LANGUAGE DeriveGeneric #-}

module Smos.Archive.Config
  ( SmosArchiveConfig(..)
  , Q
  , askWorkDir
  , module Smos.Report.Config
  , module Control.Monad.IO.Class
  , module Control.Monad.Reader
  ) where

import GHC.Generics

import Path

import Control.Monad.IO.Class
import Control.Monad.Reader

import Smos.Report.Config

data SmosArchiveConfig =
  SmosArchiveConfig
    { smosArchiveConfigReportConfig :: SmosReportConfig
    }
  deriving (Generic)

type Q = ReaderT SmosArchiveConfig IO

askWorkDir :: Q (Path Abs Dir)
askWorkDir = do
  func <-
    asks
      (agendaFileSpecGetWorkDir .
       smosReportConfigAgendaFileSpec . smosArchiveConfigReportConfig)
  liftIO func
