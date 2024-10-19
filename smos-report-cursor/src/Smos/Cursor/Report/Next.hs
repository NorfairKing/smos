{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Cursor.Report.Next where

import Control.Monad
import Cursor.Forest
import Data.Maybe
import Data.Time
import Data.Validity
import GHC.Generics (Generic)
import Lens.Micro
import Path
import Smos.Cursor.Report.Entry
import Smos.Cursor.SmosFile
import Smos.Data
import Smos.Directory.Archive
import Smos.Directory.OptParse
import Smos.Directory.ShouldPrint
import Smos.Report.Filter
import Smos.Report.Next

produceNextActionReportCursor :: Maybe EntryFilter -> HideArchive -> ShouldPrint -> DirectorySettings -> IO NextActionReportCursor
produceNextActionReportCursor mf ha sp dc =
  NextActionReportCursor <$> produceEntryReportCursor makeNextActionEntryCursor' id mf ha sp dc

data NextActionReportCursor = NextActionReportCursor
  { nextActionReportCursorEntryReportCursor :: EntryReportCursor (TodoState, UTCTime) -- The time at which the entry became a next action
  }
  deriving (Show, Eq, Generic)

instance Validity NextActionReportCursor

-- TODO validity constraint: the entry report cursor's value matches the forest cursor

nextActionReportCursorEntryReportCursorL :: Lens' NextActionReportCursor (EntryReportCursor (TodoState, UTCTime))
nextActionReportCursorEntryReportCursorL = lens nextActionReportCursorEntryReportCursor $ \wrc ne -> wrc {nextActionReportCursorEntryReportCursor = ne}

nextActionReportCursorBuildSmosFileCursor :: Path Abs Dir -> NextActionReportCursor -> Maybe (Path Abs File, SmosFileCursor)
nextActionReportCursorBuildSmosFileCursor ad = entryReportCursorBuildSmosFileCursor ad . nextActionReportCursorEntryReportCursor

nextActionReportCursorNext :: NextActionReportCursor -> Maybe NextActionReportCursor
nextActionReportCursorNext = nextActionReportCursorEntryReportCursorL entryReportCursorNext

nextActionReportCursorPrev :: NextActionReportCursor -> Maybe NextActionReportCursor
nextActionReportCursorPrev = nextActionReportCursorEntryReportCursorL entryReportCursorPrev

nextActionReportCursorFirst :: NextActionReportCursor -> NextActionReportCursor
nextActionReportCursorFirst = nextActionReportCursorEntryReportCursorL %~ entryReportCursorFirst

nextActionReportCursorLast :: NextActionReportCursor -> NextActionReportCursor
nextActionReportCursorLast = nextActionReportCursorEntryReportCursorL %~ entryReportCursorLast

nextActionReportCursorSelectReport :: NextActionReportCursor -> Maybe NextActionReportCursor
nextActionReportCursorSelectReport = nextActionReportCursorEntryReportCursorL entryReportCursorSelectReport

nextActionReportCursorSelectFilter :: NextActionReportCursor -> Maybe NextActionReportCursor
nextActionReportCursorSelectFilter = nextActionReportCursorEntryReportCursorL entryReportCursorSelectFilter

nextActionReportCursorInsert :: Char -> NextActionReportCursor -> Maybe NextActionReportCursor
nextActionReportCursorInsert c = nextActionReportCursorEntryReportCursorL $ entryReportCursorInsert c

nextActionReportCursorAppend :: Char -> NextActionReportCursor -> Maybe NextActionReportCursor
nextActionReportCursorAppend c = nextActionReportCursorEntryReportCursorL $ entryReportCursorAppend c

nextActionReportCursorRemove :: NextActionReportCursor -> Maybe NextActionReportCursor
nextActionReportCursorRemove = nextActionReportCursorEntryReportCursorL entryReportCursorRemove

nextActionReportCursorDelete :: NextActionReportCursor -> Maybe NextActionReportCursor
nextActionReportCursorDelete = nextActionReportCursorEntryReportCursorL entryReportCursorDelete

makeNextActionEntryCursor' :: Path Rel File -> ForestCursor Entry Entry -> [(TodoState, UTCTime)]
makeNextActionEntryCursor' _ = maybeToList . makeNextActionEntryCursor

makeNextActionEntryCursor :: ForestCursor Entry Entry -> Maybe (TodoState, UTCTime)
makeNextActionEntryCursor fc = parseNextActionStateTimestamp $ forestCursorCurrent fc

parseNextActionStateTimestamp :: Entry -> Maybe (TodoState, UTCTime)
parseNextActionStateTimestamp =
  firstNext
    . unStateHistory
    . entryStateHistory
  where
    firstNext :: [StateHistoryEntry] -> Maybe (TodoState, UTCTime)
    firstNext = \case
      [] -> Nothing
      (StateHistoryEntry {..} : _) -> do
        ts <- stateHistoryEntryNewState
        guard $ isNextTodoState ts
        pure (ts, stateHistoryEntryTimestamp)
