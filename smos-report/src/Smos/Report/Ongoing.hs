{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.Ongoing where

import Autodocodec
import Conduit
import Control.DeepSeq
import Cursor.Simple.Forest
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Conduit.Combinators as C
import qualified Data.Map as M
import Data.Maybe
import Data.Validity
import Data.Validity.Path ()
import GHC.Generics
import Path
import Smos.Data
import Smos.Directory.Archive
import Smos.Directory.OptParse.Types
import Smos.Directory.ShouldPrint
import Smos.Directory.Streaming
import Smos.Report.Filter

produceOngoingReport ::
  MonadIO m =>
  Maybe EntryFilter ->
  HideArchive ->
  ShouldPrint ->
  DirectorySettings ->
  m OngoingReport
produceOngoingReport ef ha sp dc =
  produceReport ha sp dc (nextActionReportConduit ef)

nextActionReportConduit ::
  Monad m =>
  Maybe EntryFilter ->
  ConduitT (Path Rel File, SmosFile) void m OngoingReport
nextActionReportConduit ef =
  OngoingReport
    <$> ( smosCursorCurrents
            .| C.map (uncurry parseOngoingEntry)
            .| sinkList
        )

makeOngoingReport :: [(Path Rel File, Entry)] -> OngoingReport
makeOngoingReport = OngoingReport . map (uncurry makeOngoingEntry)

parseOngoingEntry :: Path Rel File -> Entry -> Maybe OngoingEntry
parseOngoingEntry nextActionEntryFilePath e = do
  let nextActionEntryHeader = entryHeader e
  nextActionEntryBeginEnd <-
    parseBeginEnd
      (M.lookup "BEGIN" (entryTimestamps e))
      (M.lookup "END" (entryTimestamps e))
  pure $ OngoingEntry {..}

newtype OngoingReport = OngoingReport
  { nextActionReportEntries :: [OngoingEntry]
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec OngoingReport)

instance Validity OngoingReport

instance NFData OngoingReport

instance HasCodec OngoingReport where
  codec = dimapCodec OngoingReport nextActionReportEntries codec

data OngoingEntry = OngoingEntry
  { -- The path within the workflow directory
    nextActionEntryFilePath :: !(Path Rel File),
    nextActionEntryHeader :: !Header,
    nextActionEntryBeginEnd :: !BeginEnd
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec OngoingEntry)

instance Validity OngoingEntry

instance NFData OngoingEntry

instance HasCodec OngoingEntry where
  codec =
    object "OngoingEntry" $
      OngoingEntry
        <$> requiredField "path" "The path of the file in which this entry was found"
          .= nextActionEntryFilePath
        <*> requiredField "header" "The header of the entry"
          .= nextActionEntryHeader
        <*> objectCodec

data BeginEnd
  = OnlyBegin !Timestamp
  | OnlyEnd !Timestamp
  | BeginEnd !Timestamp !Timestamp
  deriving stock (Show, Eq, Generic)

-- deriving (FromJSON, ToJSON) via (Autodocodec BeginEnd)
--
parseBeginEnd :: Maybe Timestamp -> Maybe Timestamp -> Maybe BeginEnd
parseBeginEnd = \case
  (Nothing, Nothing) -> Nothing
  (Just begin, Nothing) -> Just $ OnlyBegin begin
  (Nothing, Just end) -> Just $ OnlyEnd end
  (Just begin, Just end) -> Just $ BeginEnd begin end

-- instance HasObjectCodec BeginEntry where
--   objectCodec = BeginEntry
--     <$> optionalField
