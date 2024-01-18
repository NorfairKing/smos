{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.Entry where

import Conduit
import Cursor.Simple.Forest
import Data.Aeson
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Validity
import Data.Yaml.Builder as Yaml
import GHC.Generics (Generic)
import Path
import Smos.Data
import Smos.Directory.Archive
import Smos.Directory.OptParse.Types
import Smos.Directory.ShouldPrint
import Smos.Directory.Streaming
import Smos.Report.Filter
import Smos.Report.Projection
import Smos.Report.Sorter

produceEntryReport ::
  (MonadIO m) =>
  Maybe EntryFilter ->
  HideArchive ->
  ShouldPrint ->
  NonEmpty Projection ->
  Maybe Sorter ->
  DirectorySettings ->
  m EntryReport
produceEntryReport ef ha sp p s dc = produceReport ha sp dc (entryReportConduit ef p s)

entryReportConduit ::
  (Monad m) =>
  Maybe EntryFilter ->
  NonEmpty Projection ->
  Maybe Sorter ->
  ConduitT (Path Rel File, SmosFile) void m EntryReport
entryReportConduit ef p s =
  makeEntryReport p . maybe id sorterSortCursorList s
    <$> ( smosFileCursors
            .| mFilterConduit ef
            .| sinkList
        )

data EntryReport = EntryReport
  { entryReportHeaders :: NonEmpty Projection,
    entryReportCells :: [NonEmpty Projectee]
  }
  deriving (Show, Eq, Generic)

instance Validity EntryReport

instance ToJSON EntryReport where
  toJSON EntryReport {..} = toJSON entryReportCells

instance ToYaml EntryReport where
  toYaml EntryReport {..} = Yaml.array $ map (Yaml.array . map toYaml . NE.toList) entryReportCells

makeEntryReport :: NonEmpty Projection -> [(Path Rel File, ForestCursor Entry)] -> EntryReport
makeEntryReport entryReportHeaders tups =
  let entryReportCells =
        flip map tups $ \(rp, e) ->
          performProjectionNE entryReportHeaders rp e
   in EntryReport {..}
