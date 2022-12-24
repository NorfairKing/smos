{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Calendar.Import.UnresolvedEvent where

import Autodocodec
import Control.Arrow (left)
import Data.Aeson (FromJSON, FromJSONKey (..), ToJSON, ToJSONKey (..))
import Data.Aeson.Types (fromJSONKeyCoerce, toJSONKeyText)
import qualified Data.DList as DList
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Validity
import GHC.Generics
import qualified ICal
import qualified ICal.Component as ICal
import qualified ICal.Conformance as ICal
import qualified ICal.ContentLine as ICal
import ICal.Extended
import qualified ICal.Parameter as ICal
import qualified ICal.Property as ICal
import qualified ICal.PropertyType.RecurrenceRule as ICal
import qualified ICal.Recurrence as ICal
import qualified ICal.UnfoldedLine as ICal
import Smos.Calendar.Import.Static

data UnresolvedEvents = UnresolvedEvents
  { unresolvedEventGroups :: !(Set UnresolvedEventGroup),
    unresolvedEventsTimeZones :: !(Map ICal.TZIDParam ICal.TimeZone)
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
