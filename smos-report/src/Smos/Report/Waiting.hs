{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.Waiting where

import Autodocodec
import Conduit
import Cursor.Simple.Forest
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Conduit.Combinators as C
import Data.List
import qualified Data.Map as M
import Data.Time
import Data.Validity
import Data.Validity.Time ()
import GHC.Generics
import Path
import Smos.Data
import Smos.Report.Archive
import Smos.Report.Config
import Smos.Report.Filter
import Smos.Report.ShouldPrint
import Smos.Report.Streaming
import Smos.Report.Time as Report

newtype WaitingReport = WaitingReport
  { waitingReportEntries :: [WaitingEntry]
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec WaitingReport)

instance Validity WaitingReport where
  validate wr@WaitingReport {..} =
    mconcat
      [ genericValidate wr,
        declare "The waiting report entries are in order" $ sortWaitingEntries waitingReportEntries == waitingReportEntries
      ]

instance HasCodec WaitingReport where
  codec = dimapCodec WaitingReport waitingReportEntries codec

produceWaitingReport :: MonadIO m => Maybe EntryFilter -> HideArchive -> ShouldPrint -> DirectoryConfig -> m WaitingReport
produceWaitingReport ef ha sp dc = produceReport ha sp dc (waitingReportConduit ef)

waitingReportConduit :: Monad m => Maybe EntryFilter -> ConduitT (Path Rel File, SmosFile) void m WaitingReport
waitingReportConduit ef =
  finishWaitingReport
    <$> ( waitingReportConduitHelper ef
            .| C.concatMap (uncurry makeWaitingEntry)
            .| sinkList
        )

waitingReportConduitHelper :: Monad m => Maybe EntryFilter -> ConduitT (Path Rel File, SmosFile) (Path Rel File, ForestCursor Entry) m ()
waitingReportConduitHelper ef =
  smosFileCursors
    .| smosFilter (maybe isWaitingFilter (FilterAnd isWaitingFilter) ef)
  where
    isWaitingFilter :: EntryFilter
    isWaitingFilter = FilterSnd $ FilterWithinCursor $ FilterEntryTodoState $ FilterMaybe False $ FilterSub waitingState

finishWaitingReport :: [WaitingEntry] -> WaitingReport
finishWaitingReport = WaitingReport . sortWaitingEntries

data WaitingEntry = WaitingEntry
  { waitingEntryHeader :: Header,
    waitingEntryTimestamp :: UTCTime,
    waitingEntryThreshold :: Maybe Time,
    waitingEntryFilePath :: Path Rel File
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec WaitingEntry)

instance Validity WaitingEntry

instance HasCodec WaitingEntry where
  codec =
    object "WaitingEntry" $
      WaitingEntry
        <$> requiredField "header" "The entry header" .= waitingEntryHeader
        <*> requiredFieldWith "timestamp" utctimeCodec "The timestamp at which this entry became WAITING" .= waitingEntryTimestamp
        <*> optionalFieldOrNull "threshold" "The threshold for 'have been waiting for too long'" .= waitingEntryThreshold
        <*> requiredField "path" "The path of the file that contained this waiting entry" .= waitingEntryFilePath

sortWaitingEntries :: [WaitingEntry] -> [WaitingEntry]
sortWaitingEntries = sortOn waitingEntryTimestamp

makeWaitingEntry :: Path Rel File -> ForestCursor Entry -> Maybe WaitingEntry
makeWaitingEntry rf fc = waitingQuadrupleToWaitingEntry <$> makeWaitingQuadruple rf fc

waitingQuadrupleToWaitingEntry :: (Path Rel File, ForestCursor Entry, UTCTime, Maybe Time) -> WaitingEntry
waitingQuadrupleToWaitingEntry (rf, fc, ts, mThreshold) =
  let e = forestCursorCurrent fc
   in WaitingEntry
        { waitingEntryHeader = entryHeader e,
          waitingEntryTimestamp = ts,
          waitingEntryThreshold = mThreshold,
          waitingEntryFilePath = rf
        }

makeWaitingQuadruple :: Path Rel File -> ForestCursor Entry -> Maybe (Path Rel File, ForestCursor Entry, UTCTime, Maybe Time)
makeWaitingQuadruple rf fc = do
  let e = forestCursorCurrent fc
  ts <- parseWaitingStateTimestamp e
  let mThreshold = M.lookup "waiting_threshold" (entryProperties e) >>= (either (const Nothing) Just . Report.parseTime . propertyValueText)
  pure (rf, fc, ts, mThreshold)

parseWaitingStateTimestamp :: Entry -> Maybe UTCTime
parseWaitingStateTimestamp =
  firstWaiting
    . unStateHistory
    . entryStateHistory
  where
    firstWaiting :: [StateHistoryEntry] -> Maybe UTCTime
    firstWaiting = \case
      [] -> Nothing
      (StateHistoryEntry {..} : _) ->
        if stateHistoryEntryNewState == Just waitingState
          then Just stateHistoryEntryTimestamp
          else Nothing

waitingState :: TodoState
waitingState = "WAITING"
