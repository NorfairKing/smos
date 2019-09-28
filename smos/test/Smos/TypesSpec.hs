{-# LANGUAGE TypeApplications #-}

module Smos.TypesSpec
  ( spec
  ) where

import TestImport

import Smos.Types
import Smos.Types.Gen ()

spec :: Spec
spec = functorSpec @MStop
