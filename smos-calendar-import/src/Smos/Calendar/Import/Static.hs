{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.Static where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Validity
import Data.Validity.Text (validateTextSingleLine)
import GHC.Generics (Generic)

-- | Whether an event blocks out the time it occupies
--
-- Not the same thing as the event's TRANSP.  TRANSP says whether the event
-- would consume time if it happened; STATUS says whether it is happening at
-- all.  Both bear on whether the time is really taken, so both feed this, and
-- collapsing either one onto the other loses a distinction the calendar drew.
data Busyness = Busy | Free
  deriving stock (Show, Eq, Ord, Generic)

instance Validity Busyness

instance HasCodec Busyness where
  codec = dimapCodec f g codec
    where
      f = \case
        True -> Busy
        False -> Free
      g = \case
        Busy -> True
        Free -> False

data Static = Static
  { staticSummary :: !(Maybe Text),
    staticDescription :: !(Maybe Text),
    staticBusy :: !Busyness,
    staticUID :: !(Maybe Text),
    staticOriginalEvent :: !(Maybe Text)
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
        <*> optionalFieldWithOmittedDefault' "busy" Busy
          .= staticBusy
        <*> optionalField' "uid"
          .= staticUID
        <*> optionalField' "originalEvent"
          .= staticOriginalEvent

emptyStatic :: Static
emptyStatic =
  Static
    { staticSummary = Nothing,
      staticDescription = Nothing,
      staticBusy = Busy,
      staticUID = Nothing,
      staticOriginalEvent = Nothing
    }
