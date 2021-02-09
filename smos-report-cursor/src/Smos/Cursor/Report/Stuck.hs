{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Cursor.Report.Stuck where

import Conduit
import Cursor.Simple.List.NonEmpty
import qualified Data.Conduit.Combinators as C
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Ord
import Data.Time
import Data.Validity
import Data.Validity.Path ()
import GHC.Generics
import Lens.Micro
import Path
import Smos.Cursor.Report.Entry
import Smos.Cursor.SmosFile
import Smos.Data
import Smos.Report.Archive
import Smos.Report.Config
import Smos.Report.Filter
import Smos.Report.Period
import Smos.Report.ShouldPrint
import Smos.Report.Streaming
import Smos.Report.Stuck

produceStuckReportCursor :: TimeZone -> HideArchive -> ShouldPrint -> DirectoryConfig -> IO StuckReportCursor
produceStuckReportCursor tz ha sp dc = produceReport ha sp dc $ stuckReportCursorConduit tz

stuckReportCursorConduit :: Monad m => TimeZone -> ConduitT (Path Rel File, SmosFile) void m StuckReportCursor
stuckReportCursorConduit tz = StuckReportCursor . fmap makeNonEmptyCursor . NE.nonEmpty . sortStuckEntries <$> (C.concatMap (uncurry $ makeStuckReportEntry tz) .| sinkList)

newtype StuckReportCursor = StuckReportCursor
  { stuckReportCursorNonEmptyCursor :: Maybe (NonEmptyCursor StuckReportEntry)
  }
  deriving (Show, Eq, Generic)

instance Validity StuckReportCursor where
  validate tsrc@StuckReportCursor {..} =
    mconcat
      [ genericValidate tsrc,
        declare "the entries are in order" $
          let es = maybe [] (NE.toList . rebuildNonEmptyCursor) stuckReportCursorNonEmptyCursor
           in sortStuckEntries es == es
      ]

stuckReportCursorNonEmptyCursorL :: Lens' StuckReportCursor (Maybe (NonEmptyCursor StuckReportEntry))
stuckReportCursorNonEmptyCursorL = lens stuckReportCursorNonEmptyCursor $ \wrc ne -> wrc {stuckReportCursorNonEmptyCursor = ne}

stuckReportCursorSelectedFile :: StuckReportCursor -> Maybe (Path Rel File)
stuckReportCursorSelectedFile src = do
  nec <- stuckReportCursorNonEmptyCursor src
  pure $ stuckReportEntryFilePath $ nonEmptyCursorCurrent nec

stuckReportCursorNext :: StuckReportCursor -> Maybe StuckReportCursor
stuckReportCursorNext = stuckReportCursorNonEmptyCursorL $ mapM nonEmptyCursorSelectNext

stuckReportCursorPrev :: StuckReportCursor -> Maybe StuckReportCursor
stuckReportCursorPrev = stuckReportCursorNonEmptyCursorL $ mapM nonEmptyCursorSelectPrev

stuckReportCursorFirst :: StuckReportCursor -> StuckReportCursor
stuckReportCursorFirst = stuckReportCursorNonEmptyCursorL %~ fmap nonEmptyCursorSelectFirst

stuckReportCursorLast :: StuckReportCursor -> StuckReportCursor
stuckReportCursorLast = stuckReportCursorNonEmptyCursorL %~ fmap nonEmptyCursorSelectLast
