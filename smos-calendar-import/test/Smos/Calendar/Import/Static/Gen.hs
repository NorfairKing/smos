{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Calendar.Import.Static.Gen where

import Data.GenValidity
import Data.GenValidity.Text (genSingleLineText)
import Smos.Calendar.Import.Static
import Smos.Data.Gen ()
import Test.QuickCheck

instance GenValid Static where
  shrinkValid = shrinkValidStructurally
  genValid =
    ( do
        staticSummary <- oneof [Just <$> genSingleLineText, pure Nothing]
        staticDescription <- genValid
        staticUID <- genValid
        staticOriginalEvent <- genValid
        pure Static {..}
    )
      `suchThat` isValid
