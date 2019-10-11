{-# LANGUAGE OverloadedStrings #-}

module Smos.Server.Handler.PostLoginSpec
  ( spec
  ) where

import Test.Hspec
import Test.Validity

import Network.HTTP.Types

import Servant.Client

import Smos.Client
import Smos.Server.TestUtils

spec :: Spec
spec =
  serverSpec $
  describe "PostLogin" $
  it "fails for any username and password if there are no users" $ \cenv ->
    forAllValid $ \l -> do
      errOrToken <- login cenv l
      case errOrToken of
        Right _ -> expectationFailure "Login should not have been allowed."
        Left le ->
          case le of
            LoginHeaderProblem _ -> expectationFailure "Headers should have been fine."
            LoginServantError ce ->
              case ce of
                FailureResponse _ fr2 ->
                  case statusCode $ responseStatusCode fr2 of
                    401 -> pure ()
                    _ -> expectationFailure "Should have been 401"
                _ -> expectationFailure "Unexpected Failure"
