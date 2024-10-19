{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.Ongoing where

import Autodocodec
import Conduit
import Control.DeepSeq
import Control.Monad
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Conduit.Combinators as C
import qualified Data.Map as M
import Data.Time
import Data.Time.Zones
import Data.Validity
import Data.Validity.Path ()
import GHC.Generics
import Path
import Smos.Data
import Smos.Directory.Archive
import Smos.Directory.OptParse
import Smos.Directory.ShouldPrint
import Smos.Directory.Streaming
import Smos.Report.Filter
import Text.Printf

produceOngoingReport ::
  (MonadIO m) =>
  TZ ->
  UTCTime ->
  Maybe EntryFilter ->
  HideArchive ->
  ShouldPrint ->
  DirectorySettings ->
  m OngoingReport
produceOngoingReport zone now ef ha sp dc =
  produceReport ha sp dc (ongoingReportConduit zone now ef)

ongoingReportConduit ::
  (Monad m) =>
  TZ ->
  UTCTime ->
  Maybe EntryFilter ->
  ConduitT (Path Rel File, SmosFile) void m OngoingReport
ongoingReportConduit zone now ef =
  OngoingReport
    <$> ( smosFileCursors
            .| C.filter (maybe (const True) filterPredicate ef)
            .| smosCursorCurrents
            .| C.concatMap (uncurry (parseOngoingEntry zone now))
            .| sinkList
        )

parseOngoingEntry :: TZ -> UTCTime -> Path Rel File -> Entry -> Maybe OngoingEntry
parseOngoingEntry zone now ongoingEntryFilePath e = do
  let ongoingEntryHeader = entryHeader e
  ongoingEntryBeginEnd <- parseMatchingBeginEnd zone now e
  pure $ OngoingEntry {..}

parseMatchingBeginEnd :: TZ -> UTCTime -> Entry -> Maybe BeginEnd
parseMatchingBeginEnd zone now e = do
  be <-
    parseBeginEnd
      (M.lookup "BEGIN" (entryTimestamps e))
      (M.lookup "END" (entryTimestamps e))
  guard $ beginEndMatches zone now be
  guard $ not $ entryIsDone e
  pure be

newtype OngoingReport = OngoingReport
  { ongoingReportEntries :: [OngoingEntry]
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec OngoingReport)

instance Validity OngoingReport

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

instance Validity BeginEnd

instance NFData BeginEnd

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

beginEndMatches :: TZ -> UTCTime -> BeginEnd -> Bool
beginEndMatches zone now be =
  let localNow = utcToLocalTimeTZ zone now
      today = localDay localNow
      beginCondition begin =
        case begin of
          TimestampDay d -> d <= today
          TimestampLocalTime lt -> lt <= localNow
      endCondition end =
        case end of
          TimestampDay d -> today < d
          TimestampLocalTime lt -> localNow < lt
   in case be of
        OnlyBegin begin -> beginCondition begin
        OnlyEnd end -> endCondition end
        BeginEnd begin end -> beginCondition begin && endCondition end

beginEndPercentageString :: LocalTime -> Timestamp -> Timestamp -> String
beginEndPercentageString nowLocal begin end =
  let today = localDay nowLocal
   in case (begin, end) of
        (TimestampDay bd, TimestampDay ed) ->
          printf "% 3d / % 3d" (diffDays today bd) (diffDays ed bd)
        _ ->
          let r :: Float
              r =
                realToFrac (diffLocalTime nowLocal (timestampLocalTime begin))
                  / realToFrac (diffLocalTime (timestampLocalTime end) (timestampLocalTime begin))
           in printf "% 3.f%%" $ 100 * r
