{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Cursor.Report.Timestamps where

import Conduit
import Cursor.Forest
import Cursor.Simple.List.NonEmpty
import qualified Data.Conduit.Combinators as C
import Data.List
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Ord
import Data.Time
import Data.Validity
import Data.Validity.Path ()
import GHC.Generics
import Lens.Micro
import Path
import Smos.Cursor.Collapse
import Smos.Cursor.Entry
import Smos.Cursor.SmosFile
import Smos.Data
import Smos.Report.Archive
import Smos.Report.Config
import Smos.Report.Filter
import Smos.Report.Period
import Smos.Report.ShouldPrint
import Smos.Report.Streaming

produceTimestampsReportCursor :: ZonedTime -> Period -> Maybe EntryFilterRel -> HideArchive -> ShouldPrint -> DirectoryConfig -> IO TimestampsReportCursor
produceTimestampsReportCursor now period mf ha sp dc = do
  wd <- liftIO $ resolveDirWorkflowDir dc
  runConduit $
    streamSmosFilesFromWorkflowRel ha dc
      .| timestampsReportCursorFromFilesConduit now period mf sp wd

timestampsReportCursorFromFilesConduit :: MonadIO m => ZonedTime -> Period -> Maybe EntryFilterRel -> ShouldPrint -> Path Abs Dir -> ConduitT (Path Rel File) void m TimestampsReportCursor
timestampsReportCursorFromFilesConduit now period mf sp wd =
  filterSmosFilesRel
    .| parseSmosFilesRel wd
    .| printShouldPrint sp
    .| timestampsReportCursorConduit now period mf

timestampsReportCursorConduit :: Monad m => ZonedTime -> Period -> Maybe EntryFilterRel -> ConduitT (Path Rel File, SmosFile) void m TimestampsReportCursor
timestampsReportCursorConduit now period mf =
  makeTimestampsReportCursor
    <$> ( smosFileCursors .| smosMFilter mf
            .| C.concatMap (uncurry makeTimestampsEntryCursor)
            .| C.filter (filterPeriodLocal now period . timestampLocalTime . timestampsEntryCursorTimestamp)
            .| sinkList
        )

data TimestampsReportCursor = TimestampsReportCursor
  { timestampsReportCursorTimestampsEntryCursors :: Maybe (NonEmptyCursor TimestampsEntryCursor)
  }
  deriving (Show, Eq, Generic)

instance Validity TimestampsReportCursor where
  validate wrc@TimestampsReportCursor {..} =
    mconcat
      [ genericValidate wrc,
        declare "The timestamps entries are in order" $
          let es = maybe [] (NE.toList . rebuildNonEmptyCursor) timestampsReportCursorTimestampsEntryCursors
           in sortTimestampEntryCursors es == es
      ]

timestampsReportCursorNonEmptyCursorL :: Lens' TimestampsReportCursor (Maybe (NonEmptyCursor TimestampsEntryCursor))
timestampsReportCursorNonEmptyCursorL = lens timestampsReportCursorTimestampsEntryCursors $ \wrc ne -> wrc {timestampsReportCursorTimestampsEntryCursors = ne}

makeTimestampsReportCursor :: [TimestampsEntryCursor] -> TimestampsReportCursor
makeTimestampsReportCursor =
  TimestampsReportCursor . fmap makeNonEmptyCursor
    . NE.nonEmpty
    . sortTimestampEntryCursors

sortTimestampEntryCursors :: [TimestampsEntryCursor] -> [TimestampsEntryCursor]
sortTimestampEntryCursors =
  sortBy (comparing (timestampLocalTime . timestampsEntryCursorTimestamp) <> comparing timestampsEntryCursorTimestampName)

timestampsReportCursorBuildSmosFileCursor :: Path Abs Dir -> TimestampsReportCursor -> Maybe (Path Abs File, SmosFileCursor)
timestampsReportCursorBuildSmosFileCursor pad wrc = do
  selected <- nonEmptyCursorCurrent <$> timestampsReportCursorTimestampsEntryCursors wrc
  let go :: ForestCursor Entry Entry -> SmosFileCursor
      go = SmosFileCursor . mapForestCursor (makeCollapseEntry . makeEntryCursor) makeCollapseEntry
  pure
    ( pad </> timestampsEntryCursorFilePath selected,
      go $ timestampsEntryCursorForestCursor selected
    )

data TimestampsEntryCursor = TimestampsEntryCursor
  { timestampsEntryCursorFilePath :: !(Path Rel File),
    timestampsEntryCursorForestCursor :: !(ForestCursor Entry Entry),
    timestampsEntryCursorTimestampName :: !TimestampName,
    timestampsEntryCursorTimestamp :: !Timestamp
  }
  deriving (Show, Eq, Generic)

instance Validity TimestampsEntryCursor where
  validate tec@TimestampsEntryCursor {..} =
    mconcat
      [ genericValidate tec,
        declare "The timestamp matches the forest cursor" $
          tec `elem` makeTimestampsEntryCursor timestampsEntryCursorFilePath timestampsEntryCursorForestCursor
      ]

makeTimestampsEntryCursor :: Path Rel File -> ForestCursor Entry Entry -> [TimestampsEntryCursor]
makeTimestampsEntryCursor path fc = do
  (name, timestamp) <- M.toList $ entryTimestamps $ forestCursorCurrent fc
  pure $
    TimestampsEntryCursor
      { timestampsEntryCursorFilePath = path,
        timestampsEntryCursorForestCursor = fc,
        timestampsEntryCursorTimestampName = name,
        timestampsEntryCursorTimestamp = timestamp
      }

timestampsReportCursorNext :: TimestampsReportCursor -> Maybe TimestampsReportCursor
timestampsReportCursorNext = timestampsReportCursorNonEmptyCursorL $ mapM nonEmptyCursorSelectNext

timestampsReportCursorPrev :: TimestampsReportCursor -> Maybe TimestampsReportCursor
timestampsReportCursorPrev = timestampsReportCursorNonEmptyCursorL $ mapM nonEmptyCursorSelectPrev

timestampsReportCursorFirst :: TimestampsReportCursor -> TimestampsReportCursor
timestampsReportCursorFirst = timestampsReportCursorNonEmptyCursorL %~ fmap nonEmptyCursorSelectFirst

timestampsReportCursorLast :: TimestampsReportCursor -> TimestampsReportCursor
timestampsReportCursorLast = timestampsReportCursorNonEmptyCursorL %~ fmap nonEmptyCursorSelectLast
