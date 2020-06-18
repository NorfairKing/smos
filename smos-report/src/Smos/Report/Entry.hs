{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.Entry where

import Conduit
import Cursor.Simple.Forest
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Validity
import GHC.Generics (Generic)
import Path
import Smos.Data
import Smos.Report.Archive
import Smos.Report.Config
import Smos.Report.Filter
import Smos.Report.Projection
import Smos.Report.ShouldPrint
import Smos.Report.Sorter
import Smos.Report.Streaming

produceReport :: MonadIO m => HideArchive -> DirectoryConfig -> ConduitM (Path Rel File, SmosFile) Void m b -> m b
produceReport ha dc rc = do
  wd <- liftIO $ resolveDirWorkflowDir dc
  runConduit $ streamSmosFilesFromWorkflowRel ha dc .| produceReportFromFiles wd .| rc

produceReportFromFiles :: MonadIO m => Path Abs Dir -> ConduitM (Path Rel File) (Path Rel File, SmosFile) m ()
produceReportFromFiles wd =
  filterSmosFilesRel
    .| parseSmosFilesRel wd
    .| printShouldPrint PrintWarning

produceEntryReport :: MonadIO m => Maybe EntryFilterRel -> HideArchive -> NonEmpty Projection -> Maybe Sorter -> DirectoryConfig -> m EntryReport
produceEntryReport ef ha p s dc = produceReport ha dc (entryReportConduit ef p s)

entryReportConduit :: Monad m => Maybe EntryFilterRel -> NonEmpty Projection -> Maybe Sorter -> ConduitT (Path Rel File, SmosFile) void m EntryReport
entryReportConduit ef p s =
  makeEntryReport p . maybe id sorterSortCursorList s
    <$> ( smosFileCursors .| smosMFilter ef .| sinkList
        )

data EntryReport
  = EntryReport
      { entryReportHeaders :: NonEmpty Projection,
        entryReportCells :: [NonEmpty Projectee]
      }
  deriving (Show, Eq, Generic)

instance Validity EntryReport

makeEntryReport :: NonEmpty Projection -> [(Path Rel File, ForestCursor Entry)] -> EntryReport
makeEntryReport entryReportHeaders tups =
  let entryReportCells =
        flip map tups $ \(rp, e) ->
          flip NE.map entryReportHeaders $ \projection -> performProjection projection rp e
   in EntryReport {..}
