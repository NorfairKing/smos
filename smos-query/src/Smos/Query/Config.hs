{-# LANGUAGE DeriveGeneric #-}

module Smos.Query.Config
    ( SmosQueryConfig(..)
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

data SmosQueryConfig = SmosQueryConfig
    { smosQueryConfigReportConfig :: SmosReportConfig
    } deriving (Generic)

type Q = ReaderT SmosQueryConfig IO

askWorkDir :: Q (Path Abs Dir)
askWorkDir = do
    func <-
        asks
            (agendaFileSpecGetWorkDir .
             smosReportConfigAgendaFileSpec . smosQueryConfigReportConfig)
    liftIO func
