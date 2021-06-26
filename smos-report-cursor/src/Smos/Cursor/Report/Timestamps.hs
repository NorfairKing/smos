{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Cursor.Report.Timestamps where

import Conduit
import Control.DeepSeq
import Cursor.Forest
import Data.List
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

produceTimestampsReportCursor :: ZonedTime -> Period -> Maybe EntryFilter -> HideArchive -> ShouldPrint -> DirectoryConfig -> IO TimestampsReportCursor
produceTimestampsReportCursor now period mf ha sp dc = TimestampsReportCursor <$> produceEntryReportCursor (makeTimestampsEntryCursorAndFilterByPeriod now period) sortTimestampEntryCursors mf ha sp dc

timestampsReportCursorConduit :: Monad m => ZonedTime -> Period -> Maybe EntryFilter -> ConduitT (Path Rel File, SmosFile) void m TimestampsReportCursor
timestampsReportCursorConduit now period mf =
  TimestampsReportCursor <$> entryReportCursorConduit (makeTimestampsEntryCursorAndFilterByPeriod now period) sortTimestampEntryCursors mf

makeTimestampsEntryCursorAndFilterByPeriod :: ZonedTime -> Period -> Path Rel File -> ForestCursor Entry Entry -> [TimestampsEntryCursor]
makeTimestampsEntryCursorAndFilterByPeriod now period rf fc =
  filter (filterTimestampsEntryCursorByPeriod now period) $ makeTimestampsEntryCursor rf fc

filterTimestampsEntryCursorByPeriod :: ZonedTime -> Period -> TimestampsEntryCursor -> Bool
filterTimestampsEntryCursorByPeriod now period = filterPeriodLocal now period . timestampLocalTime . timestampsEntryCursorTimestamp

newtype TimestampsReportCursor = TimestampsReportCursor
  { timestampsReportCursorEntryReportCursor :: EntryReportCursor TimestampsEntryCursor
  }
  deriving (Show, Eq, Generic)

instance Validity TimestampsReportCursor where
  validate tsrc@TimestampsReportCursor {..} =
    mconcat
      [ genericValidate tsrc,
        declare "the entries are in order" $
          let es = timestampsReportCursorEntryReportCursor ^. entryReportCursorEntryReportEntryCursorsL
           in sortTimestampEntryCursors es == es
      ]

instance NFData TimestampsReportCursor

emptyTimestampsReportCursor :: TimestampsReportCursor
emptyTimestampsReportCursor =
  TimestampsReportCursor
    { timestampsReportCursorEntryReportCursor = emptyEntryReportCursor
    }

finaliseTimestampsReportCursor :: [EntryReportEntryCursor TimestampsEntryCursor] -> TimestampsReportCursor
finaliseTimestampsReportCursor = TimestampsReportCursor . makeEntryReportCursor . sortTimestampEntryCursors

timestampsReportCursorEntryReportCursorL :: Lens' TimestampsReportCursor (EntryReportCursor TimestampsEntryCursor)
timestampsReportCursorEntryReportCursorL = lens timestampsReportCursorEntryReportCursor $ \wrc ne -> wrc {timestampsReportCursorEntryReportCursor = ne}

sortTimestampEntryCursors :: [EntryReportEntryCursor TimestampsEntryCursor] -> [EntryReportEntryCursor TimestampsEntryCursor]
sortTimestampEntryCursors =
  sortBy $
    mconcat
      [ comparing (timestampLocalTime . timestampsEntryCursorTimestamp . entryReportEntryCursorVal),
        comparing (timestampsEntryCursorTimestampName . entryReportEntryCursorVal)
      ]

timestampsReportCursorBuildSmosFileCursor :: Path Abs Dir -> TimestampsReportCursor -> Maybe (Path Abs File, SmosFileCursor)
timestampsReportCursorBuildSmosFileCursor ad = entryReportCursorBuildSmosFileCursor ad . timestampsReportCursorEntryReportCursor

timestampsReportCursorNext :: TimestampsReportCursor -> Maybe TimestampsReportCursor
timestampsReportCursorNext = timestampsReportCursorEntryReportCursorL entryReportCursorNext

timestampsReportCursorPrev :: TimestampsReportCursor -> Maybe TimestampsReportCursor
timestampsReportCursorPrev = timestampsReportCursorEntryReportCursorL entryReportCursorPrev

timestampsReportCursorFirst :: TimestampsReportCursor -> TimestampsReportCursor
timestampsReportCursorFirst = timestampsReportCursorEntryReportCursorL %~ entryReportCursorFirst

timestampsReportCursorLast :: TimestampsReportCursor -> TimestampsReportCursor
timestampsReportCursorLast = timestampsReportCursorEntryReportCursorL %~ entryReportCursorLast

timestampsReportCursorSelectReport :: TimestampsReportCursor -> Maybe TimestampsReportCursor
timestampsReportCursorSelectReport = timestampsReportCursorEntryReportCursorL entryReportCursorSelectReport

timestampsReportCursorSelectFilter :: TimestampsReportCursor -> Maybe TimestampsReportCursor
timestampsReportCursorSelectFilter = timestampsReportCursorEntryReportCursorL entryReportCursorSelectFilter

timestampsReportCursorInsert :: Char -> TimestampsReportCursor -> Maybe TimestampsReportCursor
timestampsReportCursorInsert c = timestampsReportCursorEntryReportCursorL $ entryReportCursorInsert c

timestampsReportCursorAppend :: Char -> TimestampsReportCursor -> Maybe TimestampsReportCursor
timestampsReportCursorAppend c = timestampsReportCursorEntryReportCursorL $ entryReportCursorAppend c

timestampsReportCursorRemove :: TimestampsReportCursor -> Maybe TimestampsReportCursor
timestampsReportCursorRemove = timestampsReportCursorEntryReportCursorL entryReportCursorRemove

timestampsReportCursorDelete :: TimestampsReportCursor -> Maybe TimestampsReportCursor
timestampsReportCursorDelete = timestampsReportCursorEntryReportCursorL entryReportCursorDelete

data TimestampsEntryCursor = TimestampsEntryCursor
  { timestampsEntryCursorTimestampName :: !TimestampName,
    timestampsEntryCursorTimestamp :: !Timestamp
  }
  deriving (Show, Eq, Generic)

instance Validity TimestampsEntryCursor

instance NFData TimestampsEntryCursor

makeTimestampsEntryCursor :: Path Rel File -> ForestCursor Entry Entry -> [TimestampsEntryCursor]
makeTimestampsEntryCursor _ fc = do
  (name, timestamp) <- M.toList $ entryTimestamps $ forestCursorCurrent fc
  pure $
    TimestampsEntryCursor
      { timestampsEntryCursorTimestampName = name,
        timestampsEntryCursorTimestamp = timestamp
      }
