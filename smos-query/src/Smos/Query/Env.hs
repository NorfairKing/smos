module Smos.Query.Env where

import Control.Monad.Reader
import Path
import Smos.CLI.Colour
import Smos.Directory.OptParse
import Smos.Directory.Resolution
import Smos.Query.OptParse.Types
import System.Exit
import System.IO
import Text.Colour
import Text.Colour.Term

data Env = Env
  { envInputHandle :: !Handle,
    envOutputHandle :: !Handle,
    envErrorHandle :: !Handle,
    envColourSettings :: !ColourSettings,
    envDirectorySettings :: !DirectorySettings
  }

type Q = ReaderT Env IO

askWorkflowDir :: Q (Path Abs Dir)
askWorkflowDir = do
  func <- asks (resolveDirWorkflowDir . envDirectorySettings)
  liftIO func

askArchiveDir :: Q (Path Abs Dir)
askArchiveDir = do
  func <- asks (resolveDirArchiveDir . envDirectorySettings)
  liftIO func

askProjectsDir :: Q (Path Abs Dir)
askProjectsDir = do
  func <- asks (resolveDirProjectsDir . envDirectorySettings)
  liftIO func

askArchivedProjectsDir :: Q (Path Abs Dir)
askArchivedProjectsDir = do
  func <- asks (resolveDirArchivedProjectsDir . envDirectorySettings)
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
