{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Cursor.Report.Stuck where

import Conduit
import Control.DeepSeq
import Cursor.Simple.List.NonEmpty
import qualified Data.Conduit.Combinators as C
import qualified Data.List.NonEmpty as NE
import Data.Time.Zones
import Data.Validity
import Data.Validity.Path ()
import GHC.Generics
import Lens.Micro
import Path
import Smos.Data
import Smos.Directory.OptParse.Types
import Smos.Report.ShouldPrint
import Smos.Report.Streaming
import Smos.Report.Stuck

produceStuckReportCursor :: TZ -> ShouldPrint -> DirectorySettings -> IO StuckReportCursor
produceStuckReportCursor zone sp dc = runConduit $ streamSmosProjects sp dc .| stuckReportCursorConduit zone

stuckReportCursorConduit :: Monad m => TZ -> ConduitT (Path Rel File, SmosFile) void m StuckReportCursor
stuckReportCursorConduit zone =
  makeStuckReportCursor
    <$> (C.concatMap (uncurry $ makeStuckReportEntry zone) .| sinkList)

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
