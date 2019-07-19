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

import GHC.Generics

import Data.Aeson
import Path

import Control.Monad.IO.Class
import Control.Monad.Reader

import Smos.Report.Config

data SmosQueryConfig =
  SmosQueryConfig
    { smosQueryConfigReportConfig :: SmosReportConfig
    , smosQueryConfigHideArchive :: HideArchive
    }
  deriving (Generic)

data HideArchive
  = HideArchive
  | Don'tHideArchive
  deriving (Show, Eq, Generic)

instance FromJSON HideArchive where
  parseJSON =
    withBool "HideArchive" $ \b ->
      pure $
      if b
        then HideArchive
        else Don'tHideArchive

type Q = ReaderT SmosQueryConfig IO

askWorkflowDir :: Q (Path Abs Dir)
askWorkflowDir = do
  func <- asks (resolveReportWorkflowDir . smosQueryConfigReportConfig)
  liftIO func

askArchiveDir :: Q (Path Abs Dir)
askArchiveDir = do
  func <- asks (resolveReportArchiveDir . smosQueryConfigReportConfig)
  liftIO func
