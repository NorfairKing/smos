{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Scheduler.History
  ( RecurrenceHistory,
    LatestActivation (..),
    readReccurrenceHistory,
    computeLastRun,
    parseSmosFileSchedule,
    parseEntrySchedule,
    addScheduleHashMetadata,
  )
where

import Conduit
import Control.Applicative
import qualified Data.Conduit.Combinators as C
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Data.Time.Zones
import Data.Tree
import Data.Validity
import Debug.Trace
import GHC.Generics (Generic)
import Path
import Safe
import Smos.Data
import Smos.Data.Types
import Smos.Directory.Archive
import Smos.Directory.OptParse
import Smos.Directory.Resolution
import Smos.Directory.ShouldPrint
import Smos.Directory.Streaming
import Smos.Report.Time (Time, timeNominalDiffTime)
import Smos.Scheduler.OptParse
import System.Cron as Cron

type RecurrenceHistory = Map ScheduleItemName LatestActivation

data LatestActivation = LatestActivation
  { latestActivationActivated :: !LocalTime,
    latestActivationClosed :: !(Maybe UTCTime)
  }
  deriving (Show, Eq, Generic)

instance Validity LatestActivation

instance Semigroup LatestActivation where
  (<>) la1@(LatestActivation a1 _) la2@(LatestActivation a2 _) =
    if a1 >= a2
      then la1
      else la2

-- TODO: Make it best-case unnecessary to read the entire history
readReccurrenceHistory :: DirectorySettings -> TZ -> IO RecurrenceHistory
readReccurrenceHistory dc zone = do
  workflowDir <- resolveDirWorkflowDir dc
  archiveDir <- resolveDirArchiveDir dc

  let go :: Path Rel File -> SmosFile -> RecurrenceHistory
      go rf sf =
        case parseSmosFileSchedule sf of
          Nothing -> M.empty
          Just h ->
            let mActivatedProperty = parseSmosFileScheduleActivated sf
             in let EarliestLatest {..} = smosFileStateChanges sf
                    mActivation = do
                      latestActivationActivated <- mActivatedProperty <|> (utcToLocalTimeTZ zone <$> earliest)
                      let latestActivationClosed = case stripProperPrefix archiveDir (workflowDir </> rf) of
                            Nothing ->
                              -- Not in archive, so definitely unfinished
                              Nothing
                            Just _ ->
                              -- In archive, so the latest entry represents completion
                              latest

                      pure LatestActivation {..}
                 in case mActivation of
                      Nothing -> M.empty
                      Just a -> M.singleton h a

  runConduit $
    streamSmosFilesFromWorkflowRel Don'tHideArchive dc
      .| parseSmosFilesRel workflowDir
      .| printShouldPrint DontPrint -- TODO make this configurable
      .| C.map (uncurry go)
      .| C.foldl (M.unionWith (<>)) M.empty

parseSmosFileSchedule :: SmosFile -> Maybe ScheduleItemName
parseSmosFileSchedule sf = case smosFileForest sf of
  [] -> Nothing
  (Node e _ : _) -> parseEntrySchedule e

parseEntrySchedule :: Entry -> Maybe ScheduleItemName
parseEntrySchedule e = M.lookup scheduleNamePropertyName (entryProperties e)

parseSmosFileScheduleActivated :: SmosFile -> Maybe LocalTime
parseSmosFileScheduleActivated sf = case smosFileForest sf of
  [] -> Nothing
  (Node e _ : _) -> parseEntryScheduleActivated e

parseEntryScheduleActivated :: Entry -> Maybe LocalTime
parseEntryScheduleActivated e = do
  pv <- M.lookup scheduleActivatedPropertyName (entryProperties e)
  parseLocalTimePropertyValue pv

addScheduleHashMetadata :: LocalTime -> ScheduleItemName -> SmosFile -> SmosFile
addScheduleHashMetadata lt n sf = makeSmosFile $ goF (smosFileForest sf)
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
scheduleActivatedPropertyName = "schedule-activated"

localTimePropertyValue :: LocalTime -> PropertyValue
localTimePropertyValue = PropertyValue . T.pack . formatTime defaultTimeLocale localTimeFormat

parseLocalTimePropertyValue :: PropertyValue -> Maybe LocalTime
parseLocalTimePropertyValue = parseTimeM False defaultTimeLocale localTimeFormat . T.unpack . propertyValueText

localTimeFormat :: String
localTimeFormat = "%F %T%Q"

computeLastRun :: RecurrenceHistory -> ScheduleItemName -> Maybe LocalTime
computeLastRun rh sih =
  latestActivationActivated <$> M.lookup sih rh

smosFileStateChanges :: SmosFile -> EarliestLatest UTCTime
smosFileStateChanges = foldMap (foldMap entryStateChanges . flatten) . smosFileForest

entryStateChanges :: Entry -> EarliestLatest UTCTime
entryStateChanges = stateHistoryStateChanges . entryStateHistory

stateHistoryStateChanges :: StateHistory -> EarliestLatest UTCTime
stateHistoryStateChanges sh =
  let l = map stateHistoryEntryTimestamp (unStateHistory sh)
   in EarliestLatest (lastMay l) (headMay l)

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
