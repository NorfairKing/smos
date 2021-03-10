{-# LANGUAGE DeriveGeneric #-}

module Smos.Query.Config
  ( SmosQueryConfig (..),
    ColourConfig (..),
    HideArchive (..),
    Q,
    askWorkflowDir,
    askArchiveDir,
    askProjectsDir,
    askArchivedProjectsDir,
    outputChunks,
    getShouldPrint,
    dieQ,
    module Smos.Report.Config,
    module Control.Monad.IO.Class,
    module Control.Monad.Reader,
    module Text.Colour,
  )
where

import Control.Monad.IO.Class
import Control.Monad.Reader
import GHC.Generics (Generic)
import Path
import Smos.Report.Archive
import Smos.Report.Config
import Smos.Report.ShouldPrint
import System.Exit
import System.IO
import Text.Colour

data SmosQueryConfig = SmosQueryConfig
  { smosQueryConfigReportConfig :: !SmosReportConfig,
    smosQueryConfigColourConfig :: !ColourConfig,
    smosQueryConfigInputHandle :: !Handle,
    smosQueryConfigOutputHandle :: !Handle,
    smosQueryConfigErrorHandle :: !Handle
  }
  deriving (Show, Eq, Generic)

data ColourConfig = ColourConfig
  { colourConfigBicolour :: Maybe Colour
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

askProjectsDir :: Q (Path Abs Dir)
askProjectsDir = do
  func <- asks (resolveReportProjectsDir . smosQueryConfigReportConfig)
  liftIO func

askArchivedProjectsDir :: Q (Path Abs Dir)
askArchivedProjectsDir = do
  func <- asks (resolveReportArchivedProjectsDir . smosQueryConfigReportConfig)
  liftIO func

outputChunks :: [Chunk] -> Q ()
outputChunks cs = do
  h <- asks smosQueryConfigOutputHandle
  liftIO $ hPutChunks h cs

getShouldPrint :: Q ShouldPrint
getShouldPrint = PrintWarning <$> asks smosQueryConfigErrorHandle

dieQ :: String -> Q a
dieQ err = do
  errH <- asks smosQueryConfigErrorHandle
  liftIO $ hPutStrLn errH err >> exitFailure
