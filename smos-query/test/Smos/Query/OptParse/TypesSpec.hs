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
  jsonSpecOnValid @PreparedReportConfiguration
  genValidSpec @PreparedReportConfiguration
  jsonSpecOnValid @Colour
  genValidSpec @Colour
  jsonSpecOnValid @TableBackground
  genValidSpec @TableBackground
