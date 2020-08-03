{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.Event where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Text
import GHC.Generics (Generic)
import YamlParse.Applicative

data Event
  = Event
      { eventTitle :: !Text,
        eventDescription :: !Text -- Empty means no description
      }
  deriving (Show, Eq, Generic)

instance Validity Event where
  validate e@Event {..} =
    mconcat
      [ genericValidate e,
        declare "The title is nonempty" $ not $ T.null eventTitle,
        decorate "The title is a single line" $ validateTextSingleLine eventTitle
      ]

instance YamlSchema Event

instance FromJSON Event

instance ToJSON Event
