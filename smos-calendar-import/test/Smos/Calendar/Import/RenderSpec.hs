module Smos.Calendar.Import.RenderSpec
  ( spec,
  )
where

import Smos.Calendar.Import.Event.Gen ()
import Smos.Calendar.Import.Render
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "renderEventEntry" $ do
    it "produces valid results" $ producesValid3 renderEventEntry
