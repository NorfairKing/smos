{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Server.E2E.MonetisationSpec (spec) where

import Control.Monad
import Data.SemVer
import Servant.Client
import Smos.Client
import Test.Syd

spec :: Version -> TestDef '[ClientEnv] ()
spec serverVersion = when (serverVersion >= version 0 2 0 [] []) $
  describe "Monetisation" $ do
    itWithOuter "is able to fetch monetisation information" $ \cenv -> do
      _ <- runClientOrDie cenv clientGetMonetisation
      pure ()
