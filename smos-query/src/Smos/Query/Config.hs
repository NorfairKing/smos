{-# LANGUAGE DeriveGeneric #-}

module Smos.Query.Config
  ( SmosQueryConfig (..),
    HideArchive (..),
    Q,
    askWorkflowDir,
    askArchiveDir,
    askProjectsDir,
    askArchivedProjectsDir,
    putTableLn,
    putBoxLn,
    getShouldPrint,
    dieQ,
    module Smos.Report.Config,
    module Control.Monad.IO.Class,
    module Control.Monad.Reader,
  )
where

import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.ByteString as SB
import Data.Foldable
import Data.Sequence (Seq)
import GHC.Generics (Generic)
import Path
import Rainbow
import Rainbox as Box
import Smos.Report.Archive
import Smos.Report.Config
import Smos.Report.ShouldPrint
import System.Exit
import System.IO

data SmosQueryConfig = SmosQueryConfig
  { smosQueryConfigReportConfig :: !SmosReportConfig,
    smosQueryConfigInputHandle :: !Handle,
    smosQueryConfigOutputHandle :: !Handle,
    smosQueryConfigErrorHandle :: !Handle
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

putTableLn :: Seq Chunk -> Q ()
putTableLn myChunks = do
  out <- asks smosQueryConfigOutputHandle
  liftIO $ do
    printer <- byteStringMakerFromHandle out
    let bytestrings = chunksToByteStrings printer (toList myChunks)
    mapM_ (SB.hPutStr out) bytestrings

putBoxLn :: Orientation a => Box a -> Q ()
putBoxLn box = do
  out <- asks smosQueryConfigOutputHandle
  liftIO $ do
    printer <- byteStringMakerFromHandle out
    let bytestrings = chunksToByteStrings printer (toList (Box.render box))
    mapM_ (SB.hPutStr out) bytestrings

getShouldPrint :: Q ShouldPrint
getShouldPrint = PrintWarning <$> asks smosQueryConfigErrorHandle

dieQ :: String -> Q a
dieQ err = do
  errH <- asks smosQueryConfigErrorHandle
  liftIO $ hPutStrLn errH err >> exitFailure
