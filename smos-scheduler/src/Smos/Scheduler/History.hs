{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Scheduler.History
  ( RecurrenceHistory,
    LatestActivation (..),
    readReccurrenceHistory,
    computeLastRun,
    parseSmosFileScheduleMetadata,
    addScheduleMetadata,
  )
where

import Conduit
import qualified Data.Conduit.Combinators as C
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time
import Data.Tree
import Data.Validity
import GHC.Generics (Generic)
import Path
import Smos.Archive.Commands.File
import Smos.Data
import Smos.Directory.Archive
import Smos.Directory.OptParse
import Smos.Directory.Resolution
import Smos.Directory.ShouldPrint
import Smos.Directory.Streaming
import Smos.Scheduler.Schedule

type RecurrenceHistory = Map ScheduleItemName LatestActivation

data LatestActivation = LatestActivation
  { latestActivationActivated :: !LocalTime,
    latestActivationClosed :: !(Maybe LocalTime)
  }
  deriving (Show, Eq, Generic)

instance Validity LatestActivation

instance Semigroup LatestActivation where
  (<>) la1@(LatestActivation a1 _) la2@(LatestActivation a2 _) =
    if a1 >= a2
      then la1
      else la2

readReccurrenceHistory :: DirectorySettings -> IO RecurrenceHistory
readReccurrenceHistory dc = do
  workflowDir <- resolveDirWorkflowDir dc
  archiveDir <- resolveDirArchiveDir dc

  let go :: Path Rel File -> SmosFile -> RecurrenceHistory
      go rf sf = case parseSmosFileLatestActivation workflowDir archiveDir rf sf of
        Nothing -> M.empty
        Just (sn, la) -> M.singleton sn la

  runConduit $
    streamSmosFilesFromWorkflowRel Don'tHideArchive dc
      .| parseSmosFilesRel workflowDir
      .| printShouldPrint DontPrint -- TODO make this configurable
      .| C.map (uncurry go)
      .| C.foldl (M.unionWith (<>)) M.empty

parseSmosFileLatestActivation :: Path Abs Dir -> Path Abs Dir -> Path Rel File -> SmosFile -> Maybe (ScheduleItemName, LatestActivation)
parseSmosFileLatestActivation workflowDir archiveDir rf sf = do
  (sn, mActivated) <- parseSmosFileScheduleMetadata sf
  latestActivationActivated <- mActivated
  let latestActivationClosed = case stripProperPrefix archiveDir (workflowDir </> rf) of
        Nothing ->
          -- Not in archive, so definitely unfinished
          Nothing
        Just _ ->
          -- In archive, so the latest entry represents completion
          parseArchiveFileTimestamp rf

  pure (sn, LatestActivation {..})

parseSmosFileScheduleMetadata :: SmosFile -> Maybe (ScheduleItemName, Maybe LocalTime)
parseSmosFileScheduleMetadata sf = case smosFileForest sf of
  [] -> Nothing
  (Node e _ : _) -> parseEntryScheduleMetadata e

parseEntryScheduleMetadata :: Entry -> Maybe (ScheduleItemName, Maybe LocalTime)
parseEntryScheduleMetadata e = do
  let properties = entryProperties e
  name <- M.lookup scheduleNamePropertyName properties
  let mActivated = do
        pv <- M.lookup scheduleActivatedPropertyName properties
        parseLocalTimePropertyValue pv
  pure (name, mActivated)

addScheduleMetadata :: LocalTime -> ScheduleItemName -> SmosFile -> SmosFile
addScheduleMetadata lt n sf = makeSmosFile $ goF (smosFileForest sf)
  where
    goF :: Forest Entry -> Forest Entry
    goF = \case
      [] -> [Node (goE emptyEntry) []]
      (t : rest) -> goT t : rest
    goT :: Tree Entry -> Tree Entry
    goT (Node e sub) = Node (goE e) sub
    goE :: Entry -> Entry
    goE e =
      entrySetProperty scheduleActivatedPropertyName (localTimePropertyValue lt) $
        entrySetProperty scheduleNamePropertyName n e

scheduleNamePropertyName :: PropertyName
scheduleNamePropertyName = "schedule"

scheduleActivatedPropertyName :: PropertyName
scheduleActivatedPropertyName = "activated"

localTimePropertyValue :: LocalTime -> PropertyValue
localTimePropertyValue = PropertyValue . T.pack . formatTime defaultTimeLocale localTimeFormat

parseLocalTimePropertyValue :: PropertyValue -> Maybe LocalTime
parseLocalTimePropertyValue = parseTimeM False defaultTimeLocale localTimeFormat . T.unpack . propertyValueText

localTimeFormat :: String
localTimeFormat = "%F %T%Q"

computeLastRun :: RecurrenceHistory -> ScheduleItemName -> Maybe LocalTime
computeLastRun rh sih =
  latestActivationActivated <$> M.lookup sih rh

data EarliestLatest a = EarliestLatest
  { earliest :: Maybe a,
    latest :: Maybe a
  }

instance (Ord a) => Semigroup (EarliestLatest a) where
  (<>) (EarliestLatest me1 ml1) (EarliestLatest me2 ml2) =
    EarliestLatest
      { earliest = case (me1, me2) of
          (Just e1, Just e2) -> Just $ min e1 e2
          (_, Nothing) -> me1
          (Nothing, _) -> me2,
        latest = case (ml1, ml2) of
          (Just e1, Just e2) -> Just $ max e1 e2
          (_, Nothing) -> ml1
          (Nothing, _) -> ml2
      }

instance (Ord a) => Monoid (EarliestLatest a) where
  mempty = EarliestLatest Nothing Nothing
  mappend = (<>)
