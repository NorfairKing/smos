{-# LANGUAGE OverloadedStrings #-}

module Smos.Server.Handler.GetNextActionReportSpec
  ( spec,
  )
where

import Smos.Client
import Smos.Report.Next
import Smos.Server.TestUtils
import Test.Hspec
import Test.Validity

spec :: Spec
spec =
  serverSpec
    $ describe "GetNextActionReport"
    $ do
      it "produces valid resuls" $ \cenv ->
        withNewUser cenv $ \t -> do
          -- TODO set up files
          report <- testClientOrErr cenv (clientGetNextActionReport t)
          shouldBeValid report
      it "produces empty reports if there are no files" $ \cenv ->
        withNewUser cenv $ \t -> do
          report <- testClientOrErr cenv (clientGetNextActionReport t)
          report `shouldBe` NextActionReport []
