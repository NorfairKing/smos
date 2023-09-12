{-# LANGUAGE DeriveGeneric #-}

module Smos.Cursor.Report.Ongoing where

import Control.DeepSeq
import Cursor.Forest
import Data.Maybe
import Data.Time
import Data.Time.Zones
import Data.Validity
import Data.Validity.Path ()
import GHC.Generics
import Lens.Micro
import Path
import Smos.Cursor.Report.Entry
import Smos.Cursor.SmosFile
import Smos.Data
import Smos.Directory.Archive
import Smos.Directory.OptParse.Types
import Smos.Directory.ShouldPrint
import Smos.Report.Filter
import Smos.Report.Ongoing

produceOngoingReportCursor :: TZ -> UTCTime -> Maybe EntryFilter -> HideArchive -> ShouldPrint -> DirectorySettings -> IO OngoingReportCursor
produceOngoingReportCursor zone now mf ha sp dc =
  OngoingReportCursor
    <$> produceEntryReportCursor (makeOngoingEntryCursor' zone now) id mf ha sp dc

newtype OngoingReportCursor = OngoingReportCursor
  { ongoingReportCursorEntryReportCursor :: EntryReportCursor BeginEnd
  }
  deriving (Show, Eq, Generic)

instance Validity OngoingReportCursor

instance NFData OngoingReportCursor

ongoingReportCursorEntryReportCursorL :: Lens' OngoingReportCursor (EntryReportCursor BeginEnd)
ongoingReportCursorEntryReportCursorL = lens ongoingReportCursorEntryReportCursor $ \wrc ne -> wrc {ongoingReportCursorEntryReportCursor = ne}

emptyOngoingReportCursor :: OngoingReportCursor
emptyOngoingReportCursor = OngoingReportCursor {ongoingReportCursorEntryReportCursor = emptyEntryReportCursor}

finaliseOngoingReportCursor :: [EntryReportEntryCursor BeginEnd] -> OngoingReportCursor
finaliseOngoingReportCursor = OngoingReportCursor . makeEntryReportCursor

ongoingReportCursorBuildSmosFileCursor :: Path Abs Dir -> OngoingReportCursor -> Maybe (Path Abs File, SmosFileCursor)
ongoingReportCursorBuildSmosFileCursor ad = entryReportCursorBuildSmosFileCursor ad . ongoingReportCursorEntryReportCursor

ongoingReportCursorNext :: OngoingReportCursor -> Maybe OngoingReportCursor
ongoingReportCursorNext = ongoingReportCursorEntryReportCursorL entryReportCursorNext

ongoingReportCursorPrev :: OngoingReportCursor -> Maybe OngoingReportCursor
ongoingReportCursorPrev = ongoingReportCursorEntryReportCursorL entryReportCursorPrev

ongoingReportCursorFirst :: OngoingReportCursor -> OngoingReportCursor
ongoingReportCursorFirst = ongoingReportCursorEntryReportCursorL %~ entryReportCursorFirst

ongoingReportCursorLast :: OngoingReportCursor -> OngoingReportCursor
ongoingReportCursorLast = ongoingReportCursorEntryReportCursorL %~ entryReportCursorLast

ongoingReportCursorSelectReport :: OngoingReportCursor -> Maybe OngoingReportCursor
ongoingReportCursorSelectReport = ongoingReportCursorEntryReportCursorL entryReportCursorSelectReport

ongoingReportCursorSelectFilter :: OngoingReportCursor -> Maybe OngoingReportCursor
ongoingReportCursorSelectFilter = ongoingReportCursorEntryReportCursorL entryReportCursorSelectFilter

ongoingReportCursorInsert :: Char -> OngoingReportCursor -> Maybe OngoingReportCursor
ongoingReportCursorInsert c = ongoingReportCursorEntryReportCursorL $ entryReportCursorInsert c

ongoingReportCursorAppend :: Char -> OngoingReportCursor -> Maybe OngoingReportCursor
ongoingReportCursorAppend c = ongoingReportCursorEntryReportCursorL $ entryReportCursorAppend c

ongoingReportCursorRemove :: OngoingReportCursor -> Maybe OngoingReportCursor
ongoingReportCursorRemove = ongoingReportCursorEntryReportCursorL entryReportCursorRemove

ongoingReportCursorDelete :: OngoingReportCursor -> Maybe OngoingReportCursor
ongoingReportCursorDelete = ongoingReportCursorEntryReportCursorL entryReportCursorDelete

makeOngoingEntryCursor' :: TZ -> UTCTime -> Path Rel File -> ForestCursor Entry Entry -> [BeginEnd]
makeOngoingEntryCursor' zone now _ = maybeToList . makeOngoingEntryCursor zone now

makeOngoingEntryCursor :: TZ -> UTCTime -> ForestCursor Entry Entry -> Maybe BeginEnd
makeOngoingEntryCursor zone now = parseMatchingBeginEnd zone now . forestCursorCurrent
