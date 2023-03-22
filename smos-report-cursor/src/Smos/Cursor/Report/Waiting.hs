{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Cursor.Report.Waiting where

import Control.DeepSeq
import Cursor.Forest
import Data.List
import Data.Maybe
import Data.Time
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
import Smos.Report.Time
import Smos.Report.Waiting

produceWaitingReportCursor :: Maybe EntryFilter -> HideArchive -> ShouldPrint -> DirectorySettings -> IO WaitingReportCursor
produceWaitingReportCursor mf ha sp dc =
  WaitingReportCursor
    <$> produceEntryReportCursor makeWaitingEntryCursor' sortWaitingReport mf ha sp dc

newtype WaitingReportCursor = WaitingReportCursor
  { waitingReportCursorEntryReportCursor :: EntryReportCursor (UTCTime, Maybe Time) -- The time at which the entry became WAITING and the threshold
  }
  deriving (Show, Eq, Generic)

-- TODO validity constraint: the entry report cursor's value matches the forest cursor

instance Validity WaitingReportCursor where
  validate wrc@WaitingReportCursor {..} =
    mconcat
      [ genericValidate wrc,
        declare "The waiting entries are in order" $
          let es = waitingReportCursorEntryReportCursor ^. entryReportCursorEntryReportEntryCursorsL
           in sortWaitingReport es == es
      ]

instance NFData WaitingReportCursor

waitingReportCursorEntryReportCursorL :: Lens' WaitingReportCursor (EntryReportCursor (UTCTime, Maybe Time))
waitingReportCursorEntryReportCursorL = lens waitingReportCursorEntryReportCursor $ \wrc ne -> wrc {waitingReportCursorEntryReportCursor = ne}

emptyWaitingReportCursor :: WaitingReportCursor
emptyWaitingReportCursor =
  WaitingReportCursor
    { waitingReportCursorEntryReportCursor = emptyEntryReportCursor
    }

finaliseWaitingReportCursor :: [EntryReportEntryCursor (UTCTime, Maybe Time)] -> WaitingReportCursor
finaliseWaitingReportCursor = WaitingReportCursor . makeEntryReportCursor . sortWaitingReport

sortWaitingReport :: [EntryReportEntryCursor (UTCTime, Maybe Time)] -> [EntryReportEntryCursor (UTCTime, Maybe Time)]
sortWaitingReport = sortOn entryReportEntryCursorVal

waitingReportCursorBuildSmosFileCursor :: Path Abs Dir -> WaitingReportCursor -> Maybe (Path Abs File, SmosFileCursor)
waitingReportCursorBuildSmosFileCursor ad = entryReportCursorBuildSmosFileCursor ad . waitingReportCursorEntryReportCursor

waitingReportCursorNext :: WaitingReportCursor -> Maybe WaitingReportCursor
waitingReportCursorNext = waitingReportCursorEntryReportCursorL entryReportCursorNext

waitingReportCursorPrev :: WaitingReportCursor -> Maybe WaitingReportCursor
waitingReportCursorPrev = waitingReportCursorEntryReportCursorL entryReportCursorPrev

waitingReportCursorFirst :: WaitingReportCursor -> WaitingReportCursor
waitingReportCursorFirst = waitingReportCursorEntryReportCursorL %~ entryReportCursorFirst

waitingReportCursorLast :: WaitingReportCursor -> WaitingReportCursor
waitingReportCursorLast = waitingReportCursorEntryReportCursorL %~ entryReportCursorLast

waitingReportCursorSelectReport :: WaitingReportCursor -> Maybe WaitingReportCursor
waitingReportCursorSelectReport = waitingReportCursorEntryReportCursorL entryReportCursorSelectReport

waitingReportCursorSelectFilter :: WaitingReportCursor -> Maybe WaitingReportCursor
waitingReportCursorSelectFilter = waitingReportCursorEntryReportCursorL entryReportCursorSelectFilter

waitingReportCursorInsert :: Char -> WaitingReportCursor -> Maybe WaitingReportCursor
waitingReportCursorInsert c = waitingReportCursorEntryReportCursorL $ entryReportCursorInsert c

waitingReportCursorAppend :: Char -> WaitingReportCursor -> Maybe WaitingReportCursor
waitingReportCursorAppend c = waitingReportCursorEntryReportCursorL $ entryReportCursorAppend c

waitingReportCursorRemove :: WaitingReportCursor -> Maybe WaitingReportCursor
waitingReportCursorRemove = waitingReportCursorEntryReportCursorL entryReportCursorRemove

waitingReportCursorDelete :: WaitingReportCursor -> Maybe WaitingReportCursor
waitingReportCursorDelete = waitingReportCursorEntryReportCursorL entryReportCursorDelete

makeWaitingEntryCursor' :: Path Rel File -> ForestCursor Entry Entry -> [(UTCTime, Maybe Time)]
makeWaitingEntryCursor' rp = maybeToList . makeWaitingEntryCursor rp

makeWaitingEntryCursor :: Path Rel File -> ForestCursor Entry Entry -> Maybe (UTCTime, Maybe Time)
makeWaitingEntryCursor rp fc = do
  (_, _, t, mw) <- makeWaitingQuadruple rp fc
  pure (t, mw)
