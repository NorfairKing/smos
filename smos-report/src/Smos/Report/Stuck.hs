{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.Stuck where

import Data.Function
import Data.List
import Data.Maybe
import Data.Ord
import Data.Time
import Data.Tree
import Data.Validity
import GHC.Generics (Generic)
import Safe
import Smos.Data
import Smos.Report.Path

data StuckReport
  = StuckReport
      { stuckReportEntries :: [StuckReportEntry]
      }
  deriving (Show, Eq, Generic)

instance Validity StuckReport

data StuckReportEntry
  = StuckReportEntry
      { stuckReportEntryFilePath :: RootedPath,
        stuckReportEntryState :: Maybe TodoState,
        stuckReportEntryHeader :: Header,
        stuckReportEntryLatestChange :: Maybe UTCTime
      }
  deriving (Show, Eq, Generic)

instance Validity StuckReportEntry

makeStuckReport :: [StuckReportEntry] -> StuckReport
makeStuckReport = StuckReport . sortOn stuckReportEntryLatestChange

makeStuckReportEntry :: RootedPath -> SmosFile -> Maybe StuckReportEntry
makeStuckReportEntry stuckReportEntryFilePath sf = do
  e <- latestEntryInSmosFile sf
  let stuckReportEntryHeader = entryHeader e
      stuckReportEntryState = entryState e
      stuckReportEntryLatestChange = latestTimestampInEntry e
  pure StuckReportEntry {..}

latestEntryInSmosFile :: SmosFile -> Maybe Entry
latestEntryInSmosFile =
  fmap last
    . headMay
    . groupBy ((==) `on` latestTimestampInEntry)
    . sortOn (Down . latestTimestampInEntry)
    . concatMap flatten
    . smosFileForest

latestTimestampInEntry :: Entry -> Maybe UTCTime
latestTimestampInEntry Entry {..} =
  maximumMay $
    catMaybes
      [ latestStateChange entryStateHistory,
        latestClockChange entryLogbook
      ]

latestStateChange :: StateHistory -> Maybe UTCTime
latestStateChange (StateHistory shes) =
  case shes of
    [] -> Nothing
    (she : _) -> Just $ stateHistoryEntryTimestamp she

latestClockChange :: Logbook -> Maybe UTCTime
latestClockChange = \case
  LogOpen t _ -> Just t
  LogClosed les -> case les of
    [] -> Nothing
    (le : _) -> Just $ logbookEntryEnd le
