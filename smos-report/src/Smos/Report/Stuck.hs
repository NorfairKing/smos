{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.Stuck where

import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import Data.Time
import Data.Tree
import Data.Validity
import Data.Validity.Path ()
import GHC.Generics (Generic)
import Path
import Safe
import Smos.Data

data StuckReport
  = StuckReport
      { stuckReportEntries :: [StuckReportEntry]
      }
  deriving (Show, Eq, Generic)

instance Validity StuckReport

data StuckReportEntry
  = StuckReportEntry
      { stuckReportEntryFilePath :: Path Rel File,
        stuckReportEntryState :: Maybe TodoState,
        stuckReportEntryHeader :: Header,
        stuckReportEntryLatestChange :: Maybe UTCTime
      }
  deriving (Show, Eq, Generic)

instance Validity StuckReportEntry

makeStuckReport :: [StuckReportEntry] -> StuckReport
makeStuckReport = StuckReport . sortOn stuckReportEntryLatestChange

makeStuckReportEntry :: TimeZone -> Path Rel File -> SmosFile -> Maybe StuckReportEntry
makeStuckReportEntry tz stuckReportEntryFilePath sf = do
  e <- latestEntryInSmosFile tz sf
  let stuckReportEntryHeader = entryHeader e
      stuckReportEntryState = entryState e
      stuckReportEntryLatestChange = latestTimestampInEntry tz e
  pure StuckReportEntry {..}

latestEntryInSmosFile :: TimeZone -> SmosFile -> Maybe Entry
latestEntryInSmosFile tz =
  fmap last
    . headMay
    . groupBy ((==) `on` latestTimestampInEntry tz)
    . sortOn (Down . latestTimestampInEntry tz)
    . concatMap flatten
    . smosFileForest

latestTimestampInEntry :: TimeZone -> Entry -> Maybe UTCTime
latestTimestampInEntry tz e@Entry {..} =
  maximumMay
    $ catMaybes
    $ concat
      [ [ latestStateChange entryStateHistory,
          latestClockChange entryLogbook
        ],
        [ latestTimestamp tz entryTimestamps | not (isDone (entryState e))
        ]
      ]

isDone :: Maybe TodoState -> Bool
isDone (Just "CANCELLED") = True
isDone (Just "DONE") = True
isDone (Just "FAILED") = True
isDone _ = False

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

latestTimestamp :: TimeZone -> Map TimestampName Timestamp -> Maybe UTCTime
latestTimestamp tz = fmap snd . M.lookupMax . M.map (localTimeToUTC tz . timestampLocalTime)
