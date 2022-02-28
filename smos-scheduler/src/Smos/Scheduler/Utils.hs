{-# LANGUAGE OverloadedStrings #-}

module Smos.Scheduler.Utils where

import Control.Arrow
import Control.Monad
import qualified Data.ByteString as SB
import Data.Yaml as Yaml
import Path
import Path.IO
import Smos.Scheduler.OptParse
import System.Exit
import UnliftIO.IO.File

readStateFile :: Path Abs File -> IO (Maybe ScheduleState)
readStateFile sf = do
  mContents <- forgivingAbsence $ SB.readFile $ fromAbsFile sf
  case mContents of
    Nothing -> pure Nothing
    Just cts ->
      case Yaml.decodeEither' cts of
        Left err ->
          die $
            unlines
              [ unwords ["ERROR: unable to decode state file:", fromAbsFile sf],
                prettyPrintParseException err
              ]
        Right state -> pure $ Just state

writeStateFile :: Path Abs File -> ScheduleState -> IO ()
writeStateFile = writeYamlFile

readScheduleTemplate :: Path Abs File -> IO (Maybe (Either String ScheduleTemplate))
readScheduleTemplate = readYamlFile

readYamlFile :: FromJSON a => Path Abs File -> IO (Maybe (Either String a))
readYamlFile f = do
  mContents <- forgivingAbsence $ SB.readFile $ fromAbsFile f
  forM mContents $ \cts -> pure $ left prettyPrintParseException $ Yaml.decodeEither' cts

writeYamlFile :: ToJSON a => Path Abs File -> a -> IO ()
writeYamlFile p a = do
  ensureDir $ parent p
  writeBinaryFileDurableAtomic (fromAbsFile p) (Yaml.encode a)
