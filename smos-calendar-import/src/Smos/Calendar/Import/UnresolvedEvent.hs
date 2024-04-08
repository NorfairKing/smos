{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Calendar.Import.UnresolvedEvent where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Validity
import GHC.Generics
import ICal.Extended ()
import qualified ICal.Recurrence as ICal
import Smos.Calendar.Import.Static

newtype UnresolvedEvents = UnresolvedEvents {unresolvedEventGroups :: Set UnresolvedEventGroup}
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec UnresolvedEvents)

instance Validity UnresolvedEvents

instance HasCodec UnresolvedEvents where
  codec = dimapCodec UnresolvedEvents unresolvedEventGroups codec

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
