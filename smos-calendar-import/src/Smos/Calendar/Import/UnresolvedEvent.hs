{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Calendar.Import.UnresolvedEvent where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Validity
import GHC.Generics
import qualified ICal
import ICal.Extended ()
import qualified ICal.Recurrence as ICal
import Smos.Calendar.Import.Static

data UnresolvedEvents = UnresolvedEvents
  { unresolvedEventGroups :: !(Set UnresolvedEventGroup),
    unresolvedEventsTimeZones :: !(Map ICal.TZIDParam ICal.TimeZone)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec UnresolvedEvents)

instance Validity UnresolvedEvents

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
    unresolvedEvents :: !(Set ICal.EventOccurrence)
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
              <$> objectCodec .= unresolvedEventGroupStatic
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
