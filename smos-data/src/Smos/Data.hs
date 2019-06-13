{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Data
  ( module Smos.Data.Types
  , readSmosFile
  , writeSmosFile
  , parseSmosFile
  , parseSmosFileYaml
  , parseSmosFileJSON
  , parseSmosData
  , parseSmosDataYaml
  , parseSmosDataJSON
  , smosFileYamlBS
  , smosFileJSONBS
  , smosFileJSONPrettyBS
  , emptySmosFile
  , prettySmosForest
  , smosFileClockOutEverywhere
  , entryClockIn
  , entryClockOut
  , logbookClockIn
  , logbookClockOut
  , stateHistoryState
  , stateHistorySetState
  , entryState
  , entrySetState
  ) where

import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString as SB
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Data.Tree
import Data.Validity
import Data.Yaml as Yaml
import Data.Yaml.Builder as Yaml

import Control.Applicative
import Control.Arrow

import Path
import Path.IO

import Smos.Data.Types

readSmosFile :: Path Abs File -> IO (Maybe (Either String SmosFile))
readSmosFile fp = do
  mContents <- forgivingAbsence $ SB.readFile $ toFilePath fp
  case mContents of
    Nothing -> pure Nothing
    Just "" -> pure $ Just $ Right emptySmosFile
    Just contents_ -> pure $ Just $ parseSmosFile contents_

writeSmosFile :: Path Abs File -> SmosFile -> IO ()
writeSmosFile fp sf = do
  ensureDir $ parent fp
  SB.writeFile (toFilePath fp) (smosFileYamlBS sf)

parseSmosFile :: ByteString -> Either String SmosFile
parseSmosFile = parseSmosData

parseSmosFileYaml :: ByteString -> Either String SmosFile
parseSmosFileYaml = parseSmosDataYaml

parseSmosFileJSON :: ByteString -> Either String SmosFile
parseSmosFileJSON = parseSmosDataJSON

parseSmosData :: FromJSON a => ByteString -> Either String a
parseSmosData bs = parseSmosDataYaml bs <|> parseSmosDataJSON bs

parseSmosDataYaml :: FromJSON a => ByteString -> Either String a
parseSmosDataYaml = left show . Yaml.decodeEither'

parseSmosDataJSON :: FromJSON a => ByteString -> Either String a
parseSmosDataJSON = JSON.eitherDecode . LB.fromStrict

smosFileYamlBS :: SmosFile -> ByteString
smosFileYamlBS sf = Yaml.toByteString sf

smosFileJSONBS :: SmosFile -> LB.ByteString
smosFileJSONBS = JSON.encode

smosFileJSONPrettyBS :: SmosFile -> LB.ByteString
smosFileJSONPrettyBS = JSON.encodePretty

emptySmosFile :: SmosFile
emptySmosFile = SmosFile []

prettySmosForest :: Forest Entry -> String
prettySmosForest ts = unlines $ map prettySmosTree ts

prettySmosTree :: Tree Entry -> String
prettySmosTree Node {..} =
  unlines [prettySmosEntry rootLabel, prettySmosForest subForest]

prettySmosEntry :: Entry -> String
prettySmosEntry Entry {..} = T.unpack $ headerText entryHeader

smosFileClockOutEverywhere :: UTCTime -> SmosFile -> SmosFile
smosFileClockOutEverywhere now (SmosFile f) = SmosFile $ goF f
  where
    goT (Node e f_) = Node (entryClockOut now e) (goF f_)
    goF = map goT

entryClockIn :: UTCTime -> Entry -> Entry
entryClockIn now e =
  fromMaybe e $
  (\lb -> e {entryLogbook = lb}) <$> logbookClockIn now (entryLogbook e)

entryClockOut :: UTCTime -> Entry -> Entry
entryClockOut now e =
  fromMaybe e $
  (\lb -> e {entryLogbook = lb}) <$> logbookClockOut now (entryLogbook e)

logbookClockIn :: UTCTime -> Logbook -> Maybe Logbook
logbookClockIn now lb =
  case lb of
    LogClosed es ->
      let d = constructValid $ LogOpen now es
       in case es of
            [] -> d
            (LogbookEntry {..}:rest) ->
              if logbookEntryEnd == now
                then Just $ LogOpen logbookEntryStart rest
                else d
    LogOpen {} -> Nothing

logbookClockOut :: UTCTime -> Logbook -> Maybe Logbook
logbookClockOut now lb =
  case lb of
    LogClosed {} -> Nothing
    LogOpen start es -> constructValid $ LogClosed $ LogbookEntry start now : es

stateHistoryState :: StateHistory -> Maybe TodoState
stateHistoryState (StateHistory tups) =
  case tups of
    [] -> Nothing
    (StateHistoryEntry mts _:_) -> mts

stateHistorySetState ::
     UTCTime -> Maybe TodoState -> StateHistory -> Maybe StateHistory
stateHistorySetState now mts sh =
  constructValid $
  sh {unStateHistory = StateHistoryEntry mts now : unStateHistory sh}

entryState :: Entry -> Maybe TodoState
entryState = stateHistoryState . entryStateHistory

entrySetState :: UTCTime -> Maybe TodoState -> Entry -> Maybe Entry
entrySetState now mts e = do
  sh' <- stateHistorySetState now mts $ entryStateHistory e
  pure $ e {entryStateHistory = sh'}
