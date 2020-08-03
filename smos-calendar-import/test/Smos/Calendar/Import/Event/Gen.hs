{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Calendar.Import.Event.Gen where

import Data.GenValidity
import Data.GenValidity.Text (genSingleLineText)
import Smos.Calendar.Import.Event
import Smos.Data.Gen ()
import Test.QuickCheck

instance GenValid Event where
  shrinkValid = shrinkValidStructurally
  genValid =
    ( do
        eventSummary <- oneof [Just <$> genSingleLineText, pure Nothing]
        eventDescription <- genValid
        eventStart <- genValid
        eventEnd <- genValid
        pure Event {..}
    )
      `suchThat` isValid
