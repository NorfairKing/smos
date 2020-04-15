{-# LANGUAGE OverloadedStrings #-}

module Smos.Server.Handler.PostSyncSpec
  ( spec,
  )
where

import Smos.Client
import Smos.Server.TestUtils
import Test.Hspec
import Test.Validity

spec :: Spec
spec =
  serverSpec
    $ describe "PostSync"
    $ it "produces valid resuls"
    $ \cenv ->
      forAllValid $ \request ->
        withNewUser cenv $ \t -> do
          response <- testClientOrErr cenv (clientPostSync t request)
          shouldBeValid response
