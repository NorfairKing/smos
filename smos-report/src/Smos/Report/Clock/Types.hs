{-# LANGUAGE DeriveGeneric #-}

module Smos.Report.Clock.Types where

import GHC.Generics (Generic)

import Data.Function
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time
import Data.Time.Calendar.WeekDate
import Data.Validity
import Data.Validity.Path ()

import Data.Validity

import Smos.Data

import Smos.Report.Path
import Smos.Report.Period
import Smos.Report.TimeBlock

-- Note: the order of these constructors matters
data ClockResolution
    = SecondsResolution
    | MinutesResolution
    | HoursResolution
    deriving (Show, Eq, Ord, Generic)

instance Validity ClockResolution

type ClockTable = [ClockTableBlock]

type ClockTableBlock = Block Text ClockTableEntry

data ClockTableEntry = ClockTableEntry
    { clockTableEntryFile :: RootedPath
    , clockTableEntryHeader :: Header
    , clockTableEntryTime :: NominalDiffTime
    } deriving (Show, Eq, Generic)

instance Validity ClockTableEntry

-- Intermediary types

data ClockTime = ClockTime
    { clockTimeFile :: RootedPath
    , clockTimeHeader :: Header
    , clockTimeEntries :: NonEmpty LogbookEntry
    } deriving (Show, Eq, Generic)

instance Validity ClockTime

type ClockTimeBlock a = Block a ClockTime
