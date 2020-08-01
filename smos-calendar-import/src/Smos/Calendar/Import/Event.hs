{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.Event where

import Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Text ()
import GHC.Generics (Generic)

data Event
  = Event
      { eventTitle :: !Text,
        eventDescription :: !(Maybe Text)
      }
  deriving (Show, Eq, Generic)

instance Validity Event where
  validate e@Event {..} =
    mconcat
      [ genericValidate e,
        decorate "The event title is a single line" $ decorateList (T.unpack eventTitle) $ \c ->
          declare "The character is not a line separator" $ Char.generalCategory c == LineSeparator
      ]
