{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.Static where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Validity
import Data.Validity.Text (validateTextSingleLine)
import GHC.Generics (Generic)

-- | Whether a component's instances belong in the imported calendar
--
-- This is decided while picking but cannot be acted on there.  A component with
-- STATUS:CANCELLED and a RECURRENCE-ID is how "delete this one instance" is
-- written, so dropping such a component before the recurrence set is
-- reconciled makes the series' instance reappear at that time instead of
-- disappearing.  SMOS_NO_CALENDAR_IMPORT is a per-component opt-out and an
-- overriding component has a description of its own, so it has the same
-- problem.
data Inclusion = Include | Exclude
  deriving stock (Show, Eq, Ord, Generic)

instance Validity Inclusion

instance HasCodec Inclusion where
  codec = dimapCodec f g codec
    where
      f True = Include
      f False = Exclude
      g Include = True
      g Exclude = False

data Static = Static
  { staticSummary :: !(Maybe Text),
    staticDescription :: !(Maybe Text),
    staticBusy :: !Bool,
    staticUID :: !(Maybe Text),
    staticOriginalEvent :: !(Maybe Text),
    staticInclusion :: !Inclusion
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Static)

instance Validity Static where
  validate e@Static {..} =
    mconcat
      [ genericValidate e,
        decorate "The title is a single line if it exists" $ maybe valid validateTextSingleLine staticSummary
      ]

instance HasCodec Static where
  codec = object "Static" objectCodec

instance HasObjectCodec Static where
  objectCodec =
    bimapCodec prettyValidate id $
      Static
        <$> optionalField' "summary"
          .= staticSummary
        <*> optionalField' "description"
          .= staticDescription
        <*> optionalFieldWithOmittedDefault' "busy" True
          .= staticBusy
        <*> optionalField' "uid"
          .= staticUID
        <*> optionalField' "originalEvent"
          .= staticOriginalEvent
        <*> optionalFieldWithOmittedDefault' "include" Include
          .= staticInclusion

emptyStatic :: Static
emptyStatic =
  Static
    { staticSummary = Nothing,
      staticDescription = Nothing,
      staticBusy = True,
      staticUID = Nothing,
      staticOriginalEvent = Nothing,
      staticInclusion = Include
    }
