{-# LANGUAGE TypeApplications #-}

module Smos.TypesSpec
  ( spec,
  )
where

import Smos.Types
import Smos.Types.Gen ()
import TestImport

spec :: Spec
spec = functorSpec @MStop
