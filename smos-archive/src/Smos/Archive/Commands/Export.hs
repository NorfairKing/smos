{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Archive.Commands.Export (smosArchiveExport) where

import Conduit
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.ByteString as SB
import qualified Data.Conduit.Combinators as C
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Path
import Path.IO
import Smos.Archive.Commands.File (archiveTimeFormat)
import Smos.Archive.Env
import Smos.Archive.OptParse.Types
import Smos.Directory.Resolution
import Smos.Directory.Streaming
import Smos.Report.Filter
import Smos.Report.Period
import UnliftIO.IO.File

smosArchiveExport :: ExportSettings -> A ()
smosArchiveExport ExportSettings {..} = do
  dc <- asks setDirectorySettings
  archiveDir <- liftIO $ resolveDirArchiveDir dc
  now <- liftIO getZonedTime
  let today = localDay $ zonedTimeToLocalTime now
  let interval = periodInterval today exportSetPeriod
  let exportFile fp = do
        (withoutExtension, _) <- splitExtension (filename fp)
        let lastPieces = case map T.unpack (reverse (T.splitOn "_" (T.pack (toFilePath withoutExtension)))) of
              [] -> "" -- Should not happen, but fine
              [x] -> x
              [x, _] -> x
              (x : y : _) -> y <> "_" <> x

        when
          ( and
              [ case tryToParseDay lastPieces of
                  Nothing -> True -- Can't parse the day, must include it.
                  Just d -> filterIntervalDay interval d,
                maybe (const True) filterPredicate exportSetFilter fp
              ]
          )
          $ do
            let from = archiveDir </> fp
            let to = exportSetExportDir </> fp
            logInfoN "Exporting"
            logInfoN $ T.pack $ fromAbsFile from
            logInfoN "to"
            logInfoN $ T.pack $ fromAbsFile to
            liftIO $ do
              ensureDir $ parent to
              contents <- SB.readFile (fromAbsFile from)
              writeBinaryFileDurableAtomic (fromAbsFile to) contents

            when exportSetAlsoDeleteOriginals $ do
              logInfoN "and removing the original"
              liftIO $ removeFile from

  ensureDir exportSetExportDir
  runConduit $ streamSmosArchiveFiles dc .| C.mapM_ exportFile

tryToParseDay :: String -> Maybe Day
tryToParseDay s =
  let go formatString = parseTimeM False defaultTimeLocale formatString s
   in NE.head <$> NE.nonEmpty (mapMaybe go archiveTimeFormats)

-- These are formats that 'smos-archive' has used in the past.  We might as
-- well try to parse dates in these formats as well, as long as we parse from
-- new to old.
archiveTimeFormats :: [String]
archiveTimeFormats =
  [ archiveTimeFormat,
    "%F_%H:%M:%S",
    "%F"
  ]
