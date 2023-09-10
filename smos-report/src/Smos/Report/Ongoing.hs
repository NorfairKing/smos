{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.Ongoing where

import Autodocodec
import Conduit
import Control.DeepSeq
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Conduit.Combinators as C
import qualified Data.Map as M
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
  produceReport ha sp dc (ongoingReportConduit ef)

ongoingReportConduit ::
  Monad m =>
  Maybe EntryFilter ->
  ConduitT (Path Rel File, SmosFile) void m OngoingReport
ongoingReportConduit ef =
  OngoingReport
    <$> ( smosFileCursors
            .| C.filter (maybe (const True) filterPredicate ef)
            .| smosCursorCurrents
            .| C.concatMap (uncurry parseOngoingEntry)
            .| sinkList
        )

parseOngoingEntry :: Path Rel File -> Entry -> Maybe OngoingEntry
parseOngoingEntry ongoingEntryFilePath e = do
  let ongoingEntryHeader = entryHeader e
  ongoingEntryBeginEnd <-
    parseBeginEnd
      (M.lookup "BEGIN" (entryTimestamps e))
      (M.lookup "END" (entryTimestamps e))
  pure $ OngoingEntry {..}

newtype OngoingReport = OngoingReport
  { ongoingReportEntries :: [OngoingEntry]
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec OngoingReport)

instance Validity OngoingReport

instance NFData OngoingReport

instance HasCodec OngoingReport where
  codec = dimapCodec OngoingReport ongoingReportEntries codec

data OngoingEntry = OngoingEntry
  { -- The path within the workflow directory
    ongoingEntryFilePath :: !(Path Rel File),
    ongoingEntryHeader :: !Header,
    ongoingEntryBeginEnd :: !BeginEnd
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
          .= ongoingEntryFilePath
        <*> requiredField "header" "The header of the entry"
          .= ongoingEntryHeader
        <*> objectCodec
          .= ongoingEntryBeginEnd

data BeginEnd
  = OnlyBegin !Timestamp
  | OnlyEnd !Timestamp
  | BeginEnd !Timestamp !Timestamp
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec BeginEnd)

instance Validity BeginEnd

instance NFData BeginEnd

instance HasCodec BeginEnd where
  codec = object "BeginEnd" objectCodec

instance HasObjectCodec BeginEnd where
  objectCodec =
    bimapCodec
      ( \(mBegin, mEnd) -> case parseBeginEnd mBegin mEnd of
          Nothing -> Left "Either begin or end is required."
          Just be -> Right be
      )
      renderBeginEnd
      $ (,)
        <$> optionalField "begin" "begin timestamp"
          .= fst
        <*> optionalField "end" "end timestamp"
          .= snd

parseBeginEnd :: Maybe Timestamp -> Maybe Timestamp -> Maybe BeginEnd
parseBeginEnd mBegin mEnd = case (mBegin, mEnd) of
  (Nothing, Nothing) -> Nothing
  (Just begin, Nothing) -> Just $ OnlyBegin begin
  (Nothing, Just end) -> Just $ OnlyEnd end
  (Just begin, Just end) -> Just $ BeginEnd begin end

renderBeginEnd :: BeginEnd -> (Maybe Timestamp, Maybe Timestamp)
renderBeginEnd = \case
  OnlyBegin begin -> (Just begin, Nothing)
  OnlyEnd end -> (Nothing, Just end)
  BeginEnd begin end -> (Just begin, Just end)
