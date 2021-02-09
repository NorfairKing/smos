{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.Waiting where

import Conduit
import Cursor.Simple.Forest
import Data.Aeson
import qualified Data.Conduit.Combinators as C
import Data.List
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
import YamlParse.Applicative

newtype WaitingReport = WaitingReport
  { waitingReportEntries :: [WaitingEntry]
  }
  deriving (Show, Eq, Generic)

instance Validity WaitingReport where
  validate wr@WaitingReport {..} =
    mconcat
      [ genericValidate wr,
        declare "The waiting report entries are in order" $ sortWaitingEntries waitingReportEntries == waitingReportEntries
      ]

instance FromJSON WaitingReport

instance ToJSON WaitingReport

produceWaitingReport :: MonadIO m => Maybe EntryFilterRel -> HideArchive -> ShouldPrint -> DirectoryConfig -> m WaitingReport
produceWaitingReport ef ha sp dc = produceReport ha sp dc (waitingReportConduit ef)

waitingReportConduit :: Monad m => Maybe EntryFilterRel -> ConduitT (Path Rel File, SmosFile) void m WaitingReport
waitingReportConduit ef =
  finishWaitingReport
    <$> ( waitingReportConduitHelper ef
            .| C.concatMap (uncurry makeWaitingEntry)
            .| sinkList
        )

waitingReportConduitHelper :: Monad m => Maybe EntryFilterRel -> ConduitT (Path Rel File, SmosFile) (Path Rel File, ForestCursor Entry) m ()
waitingReportConduitHelper ef =
  smosFileCursors
    .| smosFilter (maybe isWaitingFilter (FilterAnd isWaitingFilter) ef)
  where
    isWaitingFilter :: EntryFilterRel
    isWaitingFilter = FilterSnd $ FilterWithinCursor $ FilterEntryTodoState $ FilterMaybe False $ FilterSub waitingState

finishWaitingReport :: [WaitingEntry] -> WaitingReport
finishWaitingReport = WaitingReport . sortWaitingEntries

data WaitingEntry = WaitingEntry
  { waitingEntryHeader :: Header,
    waitingEntryTimestamp :: UTCTime,
    waitingEntryFilePath :: Path Rel File
  }
  deriving (Show, Eq, Generic)

instance Validity WaitingEntry

instance YamlSchema WaitingEntry where
  yamlSchema =
    objectParser "WaitingEntry" $
      WaitingEntry
        <$> requiredField "header" "The entry header"
        <*> requiredFieldWith "timestamp" "The timestamp at which this entry became WAITING" utcSchema
        <*> requiredField "path" "The path of the file that contained this waiting entry"

instance FromJSON WaitingEntry where
  parseJSON = viaYamlSchema

instance ToJSON WaitingEntry where
  toJSON WaitingEntry {..} =
    object
      [ "header" .= waitingEntryHeader,
        "timestamp" .= formatTime defaultTimeLocale utcFormat waitingEntryTimestamp,
        "path" .= waitingEntryFilePath
      ]

sortWaitingEntries :: [WaitingEntry] -> [WaitingEntry]
sortWaitingEntries = sortOn waitingEntryTimestamp

makeWaitingEntry :: Path Rel File -> ForestCursor Entry -> Maybe WaitingEntry
makeWaitingEntry rf fc = waitingTripleToWaitingEntry <$> makeWaitingTriple rf fc

waitingTripleToWaitingEntry :: (Path Rel File, ForestCursor Entry, UTCTime) -> WaitingEntry
waitingTripleToWaitingEntry (rf, fc, time) =
  let e = forestCursorCurrent fc
   in WaitingEntry
        { waitingEntryHeader = entryHeader e,
          waitingEntryTimestamp = time,
          waitingEntryFilePath = rf
        }

makeWaitingTriple :: Path Rel File -> ForestCursor Entry -> Maybe (Path Rel File, ForestCursor Entry, UTCTime)
makeWaitingTriple rf fc = do
  let e = forestCursorCurrent fc
  time <- parseWaitingStateTimestamp e
  pure (rf, fc, time)

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
