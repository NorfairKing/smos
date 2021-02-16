{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Cursor.Report.Stuck where

import Conduit
import Control.DeepSeq
import Cursor.Simple.List.NonEmpty
import qualified Data.Conduit.Combinators as C
import qualified Data.List.NonEmpty as NE
import Data.Time
import Data.Validity
import Data.Validity.Path ()
import GHC.Generics
import Lens.Micro
import Path
import Smos.Data
import Smos.Report.Config
import Smos.Report.ShouldPrint
import Smos.Report.Streaming
import Smos.Report.Stuck

produceStuckReportCursor :: TimeZone -> ShouldPrint -> DirectoryConfig -> IO StuckReportCursor
produceStuckReportCursor tz sp dc = runConduit $ streamSmosProjects sp dc .| stuckReportCursorConduit tz

stuckReportCursorConduit :: Monad m => TimeZone -> ConduitT (Path Rel File, SmosFile) void m StuckReportCursor
stuckReportCursorConduit tz =
  makeStuckReportCursor
    <$> (C.concatMap (uncurry $ makeStuckReportEntry tz) .| sinkList)

emptyStuckReportCursor :: StuckReportCursor
emptyStuckReportCursor = StuckReportCursor {stuckReportCursorNonEmptyCursor = Nothing}

makeStuckReportCursor :: [StuckReportEntry] -> StuckReportCursor
makeStuckReportCursor =
  StuckReportCursor
    . fmap makeNonEmptyCursor
    . NE.nonEmpty
    . sortStuckEntries

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

instance NFData StuckReportCursor

stuckReportCursorNonEmptyCursorL :: Lens' StuckReportCursor (Maybe (NonEmptyCursor StuckReportEntry))
stuckReportCursorNonEmptyCursorL = lens stuckReportCursorNonEmptyCursor $ \wrc ne -> wrc {stuckReportCursorNonEmptyCursor = ne}

stuckReportCursorSelectedFile :: StuckReportCursor -> Maybe (Path Rel File)
stuckReportCursorSelectedFile src = do
  nec <- stuckReportCursorNonEmptyCursor src
  pure $ stuckReportEntryFilePath $ nonEmptyCursorCurrent nec

stuckReportCursorNext :: StuckReportCursor -> Maybe StuckReportCursor
stuckReportCursorNext = stuckReportCursorNonEmptyCursorL $ \mnec -> do
  nec <- mnec
  case nonEmptyCursorSelectNext nec of
    Just nec' -> Just $ Just nec'
    Nothing -> Nothing

stuckReportCursorPrev :: StuckReportCursor -> Maybe StuckReportCursor
stuckReportCursorPrev = stuckReportCursorNonEmptyCursorL $ \mnec -> do
  nec <- mnec
  case nonEmptyCursorSelectPrev nec of
    Just nec' -> Just $ Just nec'
    Nothing -> Nothing

stuckReportCursorFirst :: StuckReportCursor -> StuckReportCursor
stuckReportCursorFirst = stuckReportCursorNonEmptyCursorL %~ fmap nonEmptyCursorSelectFirst

stuckReportCursorLast :: StuckReportCursor -> StuckReportCursor
stuckReportCursorLast = stuckReportCursorNonEmptyCursorL %~ fmap nonEmptyCursorSelectLast
