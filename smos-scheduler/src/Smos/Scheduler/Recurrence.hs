{-# LANGUAGE DeriveGeneric #-}

module Smos.Scheduler.Recurrence
  ( computeNextRun,
    HaircutNextRun (..),
    haircutNextRun,
    RentNextRun (..),
    rentNextRun,
  )
where

import qualified Data.Map as M
import Data.Time
import Data.Validity
import GHC.Generics (Generic)
import Smos.Report.Time (Time, timeNominalDiffTime)
import Smos.Scheduler.History
import Smos.Scheduler.Schedule
import System.Cron as Cron

computeNextRun :: LocalTime -> RecurrenceHistory -> ScheduleItemName -> ScheduleItem -> Either HaircutNextRun RentNextRun
computeNextRun nowLocal rh sn si =
  case scheduleItemRecurrence si of
    HaircutRecurrence t -> Left $ computeNextRunHaircut rh sn t
    RentRecurrence cs -> Right $ computeNextRunRent nowLocal rh sn cs

data HaircutNextRun
  = ActivateHaircutImmediately
  | ActivateHaircutNoSoonerThan !LocalTime
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

haircutNextRun :: LatestActivation -> NominalDiffTime -> Maybe LocalTime
haircutNextRun la ndt =
  case latestActivationClosed la of
    Nothing ->
      -- Still in flight, don't reactivate
      Nothing
    Just closed ->
      -- Closed, plan next activation
      Just $ addLocalTime ndt closed

data RentNextRun
  = ActivateRentImmediatelyAsIfAt !LocalTime
  | ActivateRentNoSoonerThan !LocalTime
  | DoNotActivateRent
  deriving (Show, Generic)

instance Validity RentNextRun

computeNextRunRent :: LocalTime -> RecurrenceHistory -> ScheduleItemName -> CronSchedule -> RentNextRun
computeNextRunRent nowLocal rh sih cs =
  case M.lookup sih rh of
    Nothing ->
      case rentNextRunAfter nowLocal cs of
        Nothing -> DoNotActivateRent
        Just next -> ActivateRentImmediatelyAsIfAt next
    Just la -> case rentNextRun la cs of
      Just next -> ActivateRentNoSoonerThan next
      Nothing -> DoNotActivateRent

rentNextRun :: LatestActivation -> CronSchedule -> Maybe LocalTime
rentNextRun la = rentNextRunAfter (latestActivationActivated la)

rentNextRunAfter :: LocalTime -> CronSchedule -> Maybe LocalTime
rentNextRunAfter lastActivated cs = utcToLocalTime utc <$> Cron.nextMatch cs (localTimeToUTC utc lastActivated)
