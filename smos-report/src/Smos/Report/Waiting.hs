{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.Waiting where

import Conduit
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
import Smos.Report.Streaming
import YamlParse.Applicative

newtype WaitingReport
  = WaitingReport
      { waitingReportEntries :: [WaitingActionEntry]
      }
  deriving (Show, Eq, Generic)

instance Validity WaitingReport where
  validate wr@WaitingReport {..} =
    mconcat
      [ genericValidate wr,
        declare "The waiting report entries are in order" $ sortOn waitingActionEntryTimestamp waitingReportEntries == waitingReportEntries
      ]

instance FromJSON WaitingReport

instance ToJSON WaitingReport

produceWaitingReport :: MonadIO m => Maybe EntryFilterRel -> HideArchive -> DirectoryConfig -> m WaitingReport
produceWaitingReport ef ha dc = produceReport ha dc (waitingReportConduit ef)

waitingReportConduit :: Monad m => Maybe EntryFilterRel -> ConduitT (Path Rel File, SmosFile) void m WaitingReport
waitingReportConduit ef =
  finishWaitingReport . WaitingReport
    <$> ( smosFileCursors
            .| smosFilter (maybe isWaitingFilter (FilterAnd isWaitingFilter) ef)
            .| smosCursorCurrents
            .| C.map (uncurry makeWaitingActionEntry)
            .| sinkList
        )
  where
    isWaitingFilter :: EntryFilterRel
    isWaitingFilter = FilterSnd $ FilterWithinCursor $ FilterEntryTodoState $ FilterMaybe False $ FilterSub "WAITING"
    finishWaitingReport :: WaitingReport -> WaitingReport
    finishWaitingReport wr = wr {waitingReportEntries = sortOn waitingActionEntryTimestamp (waitingReportEntries wr)}

data WaitingActionEntry
  = WaitingActionEntry
      { waitingActionEntryHeader :: Header,
        waitingActionEntryTimestamp :: Maybe UTCTime,
        waitingActionEntryFilePath :: Path Rel File
      }
  deriving (Show, Eq, Generic)

instance Validity WaitingActionEntry

instance YamlSchema WaitingActionEntry where
  yamlSchema =
    objectParser "WaitingActionEntry" $
      WaitingActionEntry
        <$> requiredField "header" "The entry header"
        <*> optionalFieldWith "timestamp" "The timestamp at which this entry became WAITING" utcSchema
        <*> requiredField "path" "The path of the file that contained this waiting entry"

instance FromJSON WaitingActionEntry where
  parseJSON = viaYamlSchema

instance ToJSON WaitingActionEntry where
  toJSON WaitingActionEntry {..} =
    object
      [ "header" .= waitingActionEntryHeader,
        "timestamp" .= (formatTime defaultTimeLocale utcFormat <$> waitingActionEntryTimestamp),
        "path" .= waitingActionEntryFilePath
      ]

makeWaitingActionEntry :: Path Rel File -> Entry -> WaitingActionEntry
makeWaitingActionEntry rp Entry {..} =
  let time =
        case unStateHistory entryStateHistory of
          [] -> Nothing
          x : _ -> Just $ stateHistoryEntryTimestamp x
   in WaitingActionEntry
        { waitingActionEntryHeader = entryHeader,
          waitingActionEntryTimestamp = time,
          waitingActionEntryFilePath = rp
        }

isWaitingAction :: Entry -> Bool
isWaitingAction entry = entryState entry == Just "WAITING"
