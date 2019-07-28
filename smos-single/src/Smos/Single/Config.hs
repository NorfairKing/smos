{-# LANGUAGE DeriveGeneric #-}

module Smos.Single.Config
  ( SmosSingleConfig(..)
  , Q
  , askWorkflowDir
  , module Smos.Report.Config
  , module Control.Monad.IO.Class
  , module Control.Monad.Reader
  ) where

import GHC.Generics

import Path

import Control.Monad.IO.Class
import Control.Monad.Reader

import Smos.Report.Config

data SmosSingleConfig =
  SmosSingleConfig
    { smosSingleConfigReportConfig :: SmosReportConfig
    }
  deriving (Generic)

type Q = ReaderT SmosSingleConfig IO

askWorkflowDir :: Q (Path Abs Dir)
askWorkflowDir = do
  rc <- asks smosSingleConfigReportConfig
  liftIO $ resolveReportWorkflowDir rc
