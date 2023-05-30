{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Calendar.Import.EditedEvent where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Validity
import GHC.Generics
import ICal.Extended ()
import qualified ICal.Recurrence as ICal
import Smos.Calendar.Import.Static

newtype EditedEvents = EditedEvents {unresolvedEventGroups :: Set EditedEventGroup}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec EditedEvents)

instance Validity EditedEvents

instance HasCodec EditedEvents where
  codec = dimapCodec EditedEvents unresolvedEventGroups codec

data EditedEventGroup = EditedEventGroup
  { unresolvedEventGroupStatic :: !Static,
    unresolvedEvents :: !(Set ICal.EventOccurrence)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec EditedEventGroup)

instance Validity EditedEventGroup

instance HasCodec EditedEventGroup where
  codec =
    dimapCodec f g $
      eitherCodec
        ( object "EditedEventGroup" $
            EditedEventGroup
              <$> objectCodec
                .= unresolvedEventGroupStatic
              <*> optionalFieldWithOmittedDefault' "events" S.empty
                .= unresolvedEvents
        )
        codec
    where
      f = \case
        Left ueg -> ueg
        Right s -> EditedEventGroup emptyStatic s
      g ueg =
        if unresolvedEventGroupStatic ueg == emptyStatic
          then Right (unresolvedEvents ueg)
          else Left ueg
