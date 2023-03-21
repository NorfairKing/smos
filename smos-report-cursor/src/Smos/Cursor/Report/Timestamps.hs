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

produceTimestampsReportCursor :: Day -> Period -> Maybe EntryFilter -> HideArchive -> ShouldPrint -> DirectorySettings -> IO TimestampsReportCursor
produceTimestampsReportCursor today period mf ha sp dc =
  TimestampsReportCursor <$> produceEntryReportCursor (makeTimestampsEntryCursorAndFilterByPeriod today period) sortTimestampEntryCursors mf ha sp dc

timestampsReportCursorConduit :: Monad m => Day -> Period -> Maybe EntryFilter -> ConduitT (Path Rel File, SmosFile) void m TimestampsReportCursor
timestampsReportCursorConduit today period mf =
  TimestampsReportCursor <$> entryReportCursorConduit (makeTimestampsEntryCursorAndFilterByPeriod today period) sortTimestampEntryCursors mf

makeTimestampsEntryCursorAndFilterByPeriod :: Day -> Period -> Path Rel File -> ForestCursor Entry Entry -> [TimestampsEntryCursor]
makeTimestampsEntryCursorAndFilterByPeriod today period rf fc =
  filter (filterTimestampsEntryCursorByPeriod today period) $ makeTimestampsEntryCursor rf fc

filterTimestampsEntryCursorByPeriod :: Day -> Period -> TimestampsEntryCursor -> Bool
filterTimestampsEntryCursorByPeriod today period =
  let interval = periodInterval today period
   in filterIntervalTimestamp interval . timestampsEntryCursorTimestamp

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
