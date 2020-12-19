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
  describe "renderEvent" $ do
    it "produces valid results" $ producesValidsOnValids2 renderEvent
