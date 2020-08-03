{-# LANGUAGE DeriveGeneric #-}

module Smos.Calendar.Import.RecurringEvent where

import Data.Aeson
import Data.Validity
import GHC.Generics
import Text.ICalendar.Types
import YamlParse.Applicative

data RecurringEvent = RecurringEvent
  deriving (Show, Eq, Generic)

instance Validity RecurringEvent

instance YamlSchema RecurringEvent

instance FromJSON RecurringEvent

instance ToJSON RecurringEvent
