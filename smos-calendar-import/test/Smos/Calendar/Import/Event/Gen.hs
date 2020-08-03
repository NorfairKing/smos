{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.Event.Gen where

import Data.Char as Char
import Data.GenValidity
import Data.GenValidity.Text (genSingleLineText)
import qualified Data.Text as T
import Smos.Calendar.Import.Event
import Test.QuickCheck

instance GenValid Event where
  shrinkValid = shrinkValidStructurally
  genValid =
    ( do
        eventSummary <- oneof [Just <$> genSingleLineText, pure Nothing]
        eventDescription <- genValid
        pure Event {..}
    )
      `suchThat` isValid
