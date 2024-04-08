{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.Stuck where

import Control.DeepSeq
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import Data.Time
import Data.Time.Zones
import Data.Tree
import Data.Validity
import Data.Validity.Path ()
import GHC.Generics (Generic)
import Path
import Safe
import Smos.Data

data StuckReport = StuckReport
  { stuckReportEntries :: [StuckReportEntry]
  }
  deriving (Show, Generic)

instance Validity StuckReport

data StuckReportEntry = StuckReportEntry
  { stuckReportEntryFilePath :: Path Rel File,
    stuckReportEntryState :: Maybe TodoState,
    stuckReportEntryHeader :: Header,
    stuckReportEntryLatestChange :: Maybe UTCTime
  }
  deriving (Show, Eq, Generic)

instance Validity StuckReportEntry

instance NFData StuckReportEntry

makeStuckReport :: [StuckReportEntry] -> StuckReport
makeStuckReport = StuckReport . sortStuckEntries

sortStuckEntries :: [StuckReportEntry] -> [StuckReportEntry]
sortStuckEntries = sortOn stuckReportEntryLatestChange

makeStuckReportEntry :: TZ -> Path Rel File -> SmosFile -> Maybe StuckReportEntry
makeStuckReportEntry zone stuckReportEntryFilePath sf = do
  e <- latestEntryInSmosFile zone sf
  let stuckReportEntryHeader = entryHeader e
      stuckReportEntryState = entryState e
      stuckReportEntryLatestChange = latestTimestampInEntry zone e
  pure StuckReportEntry {..}

latestEntryInSmosFile :: TZ -> SmosFile -> Maybe Entry
latestEntryInSmosFile zone =
  (>>= lastMay)
    . headMay
    . groupBy ((==) `on` latestTimestampInEntry zone)
    . sortOn (Down . latestTimestampInEntry zone)
    . concatMap flatten
    . smosFileForest

latestTimestampInEntry :: TZ -> Entry -> Maybe UTCTime
latestTimestampInEntry zone e@Entry {..} =
  maximumMay $
    catMaybes $
      concat
        [ [ latestStateChange entryStateHistory,
            latestClockChange entryLogbook
          ],
          [ latestTimestamp zone entryTimestamps | not (entryIsDone e)
          ]
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

latestTimestamp :: TZ -> Map TimestampName Timestamp -> Maybe UTCTime
latestTimestamp zone = fmap snd . M.lookupMax . M.map (localTimeToUTCTZ zone . timestampLocalTime)
