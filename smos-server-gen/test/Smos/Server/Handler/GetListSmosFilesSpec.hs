module Smos.Server.Handler.GetListSmosFilesSpec
  ( spec,
  )
where

import Smos.Client
import Smos.Data.Gen ()
import Smos.Server.InterestingStore
import Smos.Server.TestUtils
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec =
  describe "GetListSmosFiles" $ do
    serverSpec $
      it "produces a valid smos directory forest for any interesting store" $
        \cenv ->
          forAllValid $ \store ->
            withNewUser cenv $ \t -> do
              testClientOrErr cenv $ setupInterestingStore t store
              dirforest <- testClientOrErr cenv $ clientGetListSmosFiles t
              shouldBeValid dirforest
