{-# LANGUAGE DeriveGeneric #-}

module Smos.Query.Env where

import Control.Monad.Reader
import GHC.Generics (Generic)
import Path
import Smos.CLI.Colour
import Smos.Query.OptParse.Types
import Smos.Report.Config
import System.Exit
import System.IO
import Text.Colour
import Text.Colour.Term

data Env = Env
  { envInputHandle :: !Handle,
    envOutputHandle :: !Handle,
    envErrorHandle :: !Handle,
    envColourSettings :: !ColourSettings,
    envDirectoryConfig :: !DirectoryConfig
  }
  deriving (Show, Eq, Generic)

type Q = ReaderT Env IO

askWorkflowDir :: Q (Path Abs Dir)
askWorkflowDir = do
  func <- asks (resolveDirWorkflowDir . envDirectoryConfig)
  liftIO func

askArchiveDir :: Q (Path Abs Dir)
askArchiveDir = do
  func <- asks (resolveDirArchiveDir . envDirectoryConfig)
  liftIO func

askProjectsDir :: Q (Path Abs Dir)
askProjectsDir = do
  func <- asks (resolveDirProjectsDir . envDirectoryConfig)
  liftIO func

askArchivedProjectsDir :: Q (Path Abs Dir)
askArchivedProjectsDir = do
  func <- asks (resolveDirArchivedProjectsDir . envDirectoryConfig)
  liftIO func

outputChunks :: [Chunk] -> Q ()
outputChunks cs = do
  h <- asks envOutputHandle
  liftIO $ hPutChunksLocale h cs

getShouldPrint :: Q ShouldPrint
getShouldPrint = PrintWarning <$> asks envErrorHandle

dieQ :: String -> Q a
dieQ err = do
  errH <- asks envErrorHandle
  liftIO $ hPutStrLn errH err >> exitFailure
