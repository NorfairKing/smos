{-# LANGUAGE TypeApplications #-}

module Smos.Query.OptParse.TypesSpec (spec) where

import Smos.Query.OptParse.Gen ()
import Smos.Query.OptParse.Types
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson
import Text.Colour.Chunk
import Text.Colour.Layout

spec :: Spec
spec = do
  jsonSpec @PreparedReportConfiguration
  genValidSpec @PreparedReportConfiguration
  jsonSpec @Colour
  genValidSpec @Colour
  jsonSpec @TableBackground
  genValidSpec @TableBackground
