module Smos.Data.Types where

import Data.Maybe
import Data.List
import Data.Map
import Data.Set
import Data.DateTime
import Data.Date

type Entry
  = { header :: Header
    , contents :: Maybe Contents
    , timestamps :: Map TimestampName Timestamp
    , properties :: Map PropertyName PropertyValue
    , stateHistory :: StateHistory
    , tags :: Set Tag
    , logbook :: Logbook
    }

type Header
  = String

type Contents
  = String

type TimestampName
  = String

data Timestamp
  = TimestampDay Date
  | TimestampLocalTime DateTime

type PropertyName
  = String

type PropertyValue
  = String

type StateHistory
  = List StateHistoryEntry

type StateHistoryEntry
  = { newState :: Maybe TodoState
    , dateTime :: DateTime
    }

type Tag
  = String

type TodoState
  = String

data Logbook
  = LogOpen DateTime (List LogbookEntry)
  | LogClosed (List LogbookEntry)

type LogbookEntry
  = { start :: DateTime
    , end :: DateTime
    }
