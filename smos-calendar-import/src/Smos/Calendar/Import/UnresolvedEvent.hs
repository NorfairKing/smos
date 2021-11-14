{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.UnresolvedEvent where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Validity
import GHC.Generics
import Smos.Calendar.Import.Static
import Smos.Calendar.Import.TimeZone
import Smos.Calendar.Import.UnresolvedTimestamp

data UnresolvedEvents = UnresolvedEvents
  { unresolvedEventGroups :: !(Set UnresolvedEventGroup),
    unresolvedEventsTimeZones :: !(Map TimeZoneId TimeZoneHistory)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec UnresolvedEvents)

instance Validity UnresolvedEvents

-- TODO validity constraints on timezone ids

instance HasCodec UnresolvedEvents where
  codec =
    dimapCodec f g $
      eitherCodec
        ( object "UnresolvedEvents" $
            UnresolvedEvents
              <$> requiredField' "events" .= unresolvedEventGroups
              <*> optionalFieldWithOmittedDefault' "zones" M.empty .= unresolvedEventsTimeZones
        )
        codec
    where
      f = \case
        Left ues -> ues
        Right gs -> UnresolvedEvents gs M.empty
      g ues =
        if null (unresolvedEventsTimeZones ues)
          then Right (unresolvedEventGroups ues)
          else Left ues

data UnresolvedEventGroup = UnresolvedEventGroup
  { unresolvedEventGroupStatic :: !Static,
    unresolvedEvents :: !(Set UnresolvedEvent)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec UnresolvedEventGroup)

instance Validity UnresolvedEventGroup

instance HasCodec UnresolvedEventGroup where
  codec =
    dimapCodec f g $
      eitherCodec
        ( object "UnresolvedEventGroup" $
            UnresolvedEventGroup
              <$> staticObjectCodec .= unresolvedEventGroupStatic
              <*> optionalFieldWithOmittedDefault' "events" S.empty .= unresolvedEvents
        )
        codec
    where
      f = \case
        Left ueg -> ueg
        Right s -> UnresolvedEventGroup emptyStatic s
      g ueg =
        if unresolvedEventGroupStatic ueg == emptyStatic
          then Right (unresolvedEvents ueg)
          else Left ueg

data UnresolvedEvent = UnresolvedEvent
  { unresolvedEventStart :: !(Maybe CalTimestamp),
    unresolvedEventEnd :: !(Maybe CalEndDuration)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec UnresolvedEvent)

instance Validity UnresolvedEvent

instance HasCodec UnresolvedEvent where
  codec =
    object "UnresolvedEvent" $
      UnresolvedEvent
        <$> optionalField' "start" .= unresolvedEventStart
        <*> optionalField' "end" .= unresolvedEventEnd
