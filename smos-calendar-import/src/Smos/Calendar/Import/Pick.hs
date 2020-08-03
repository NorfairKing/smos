module Smos.Calendar.Import.Pick where

import Data.Validity
import GHC.Generics
import Smos.Calendar.Import.RecurringEvent
import Text.ICalendar.Types

pickEvents :: [VCalendar] -> [RecurringEvent]
pickEvents _ = []
