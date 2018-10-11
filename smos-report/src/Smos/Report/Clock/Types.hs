{-# LANGUAGE DeriveGeneric #-}

module Smos.Report.Clock.Types where
import GHC.Generics(Generic)

import Data.Validity

data ClockPeriod
    = Today
    | ThisWeek
    | AllTime
    deriving (Show, Eq, Generic)

instance Validity ClockPeriod

-- Note: the order of these constructors matters
data ClockResolution
    = SecondsResolution
    | MinutesResolution
    | HoursResolution
    deriving (Show, Eq, Ord, Generic)

instance Validity ClockResolution
