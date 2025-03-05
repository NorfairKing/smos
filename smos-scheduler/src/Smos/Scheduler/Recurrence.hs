{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Scheduler.Recurrence
  ( computeLastRun,
    computeNextRun,
    HaircutNextRun (..),
    computeNextRunHaircut,
    haircutNextRun,
    RentNextRun (..),
    computeNextRunRent,
    rentNextRun,
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
import Smos.Scheduler.History
import Smos.Scheduler.OptParse
import Smos.Scheduler.Schedule
import System.Cron as Cron

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

computeNextRun :: TZ -> UTCTime -> RecurrenceHistory -> ScheduleItemName -> ScheduleItem -> Either HaircutNextRun RentNextRun
computeNextRun zone now rh sn si =
  case scheduleItemRecurrence si of
    HaircutRecurrence t -> Left $ computeNextRunHaircut rh sn t
    RentRecurrence cs -> Right $ computeNextRunRent zone now rh sn cs

data HaircutNextRun
  = ActivateHaircutImmediately
  | ActivateHaircutNoSoonerThan !UTCTime
  | DoNotActivateHaircut
  deriving (Show, Generic)

instance Validity HaircutNextRun

computeNextRunHaircut :: RecurrenceHistory -> ScheduleItemName -> Time -> HaircutNextRun
computeNextRunHaircut rh sih t =
  case M.lookup sih rh of
    Nothing -> ActivateHaircutImmediately
    Just la -> case haircutNextRun la (timeNominalDiffTime t) of
      Just next -> ActivateHaircutNoSoonerThan next
      Nothing -> DoNotActivateHaircut

haircutNextRun :: LatestActivation -> NominalDiffTime -> Maybe UTCTime
haircutNextRun la ndt =
  case latestActivationClosed la of
    Nothing ->
      -- Still in flight, don't reactivate
      Nothing
    Just closed ->
      -- Closed, plan next activation
      Just $ addUTCTime ndt closed

data RentNextRun
  = ActivateRentImmediatelyAsIfAt !LocalTime
  | ActivateRentNoSoonerThan !LocalTime
  | DoNotActivateRent
  deriving (Show, Generic)

instance Validity RentNextRun

computeNextRunRent :: TZ -> UTCTime -> RecurrenceHistory -> ScheduleItemName -> CronSchedule -> RentNextRun
computeNextRunRent zone now rh sih cs =
  case M.lookup sih rh of
    Nothing ->
      case rentNextRunAfter (utcToLocalTimeTZ zone now) cs of
        Nothing -> DoNotActivateRent
        Just next -> ActivateRentImmediatelyAsIfAt next
    Just la -> case rentNextRun la cs of
      Just next -> ActivateRentNoSoonerThan next
      Nothing -> DoNotActivateRent

rentNextRun :: LatestActivation -> CronSchedule -> Maybe LocalTime
rentNextRun la = rentNextRunAfter (latestActivationActivated la)

rentNextRunAfter :: LocalTime -> CronSchedule -> Maybe LocalTime
rentNextRunAfter lastActivated cs = utcToLocalTime utc <$> Cron.nextMatch cs (localTimeToUTC utc lastActivated)

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
