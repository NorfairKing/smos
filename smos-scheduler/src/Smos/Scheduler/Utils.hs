module Smos.Scheduler.Utils where

import Control.Arrow
import Control.Monad
import qualified Data.ByteString as SB
import Data.Yaml as Yaml
import Path
import Path.IO
import Smos.Scheduler.OptParse
import UnliftIO.IO.File

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
