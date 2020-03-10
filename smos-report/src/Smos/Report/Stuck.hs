{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Smos.Report.Stuck where

import GHC.Generics (Generic)

import Data.List
import Data.Ord
import Data.Time
import Data.Tree
import Data.Validity

import Smos.Data

import Smos.Report.Path

data StuckReport =
  StuckReport
    { stucReportEntries :: [StuckReportEntry]
    }
  deriving (Show, Eq, Generic)

instance Validity StuckReport

data StuckReportEntry =
  StuckReportEntry
    { stuckReportEntryFilePath :: RootedPath
    , stuckReportEntryHeader :: Header
    , stuckReportEntryState :: Maybe TodoState
    , stuckReportEntryLatestChange :: Maybe UTCTime
    }
  deriving (Show, Eq, Generic)

instance Validity StuckReportEntry

makeStuckReportEntry :: RootedPath -> SmosFile -> Maybe StuckReportEntry
makeStuckReportEntry stuckReportEntryFilePath sf = do
  e <- latestEntryInSmosFile sf
  let stuckReportEntryHeader = entryHeader e
      stuckReportEntryState = entryState e
      stuckReportEntryLatestChange = latestStateChange $ entryStateHistory e
  pure StuckReportEntry {..}

latestEntryInSmosFile :: SmosFile -> Maybe Entry
latestEntryInSmosFile =
  headMay .
  sortOn (Down . latestStateChange . entryStateHistory) . concatMap flatten . smosFileForest
  where
    headMay :: [a] -> Maybe a
    headMay [] = Nothing
    headMay (h:_) = Just h

latestStateChange :: StateHistory -> Maybe UTCTime
latestStateChange (StateHistory shes) =
  case shes of
    [] -> Nothing
    (she:_) -> Just $ stateHistoryEntryTimestamp she
