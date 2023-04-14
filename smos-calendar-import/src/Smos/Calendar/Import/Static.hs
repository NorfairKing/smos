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

data Static = Static
  { staticSummary :: !(Maybe Text),
    staticDescription :: !(Maybe Text),
    staticBusy :: !Bool,
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
        <*> optionalFieldWithOmittedDefault' "busy" True
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
      staticBusy = True,
      staticUID = Nothing,
      staticOriginalEvent = Nothing
    }
