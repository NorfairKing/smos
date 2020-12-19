{-# LANGUAGE OverloadedStrings #-}

module Smos.Server.Handler.GetNextActionReportSpec
  ( spec,
  )
where

import Smos.Client
import Smos.Report.Next
import Smos.Server.InterestingStore
import Smos.Server.TestUtils
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec =
  serverSpec $
    describe "GetNextActionReport" $
      do
        it "produces empty reports if there are no files" $ \cenv ->
          withNewUser cenv $ \t -> do
            report <- testClientOrErr cenv (clientGetNextActionReport t)
            report `shouldBe` NextActionReport []
        it "produces valid resuls" $ \cenv ->
          forAllValid $ \store ->
            withNewUser cenv $ \t -> do
              testClientOrErr cenv $ setupInterestingStore t store
              report <- testClientOrErr cenv (clientGetNextActionReport t)
              shouldBeValid report
