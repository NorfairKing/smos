{-# LANGUAGE OverloadedStrings #-}

module Smos.Server.Handler.PostLoginSpec
  ( spec,
  )
where

import Network.HTTP.Types
import Servant.Client
import Smos.Client
import Smos.Server.TestUtils
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec =
  serverSpec $
    describe "PostLogin" $
      do
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
        it "succeeds after registering" $ \cenv ->
          forAllValid $ \r -> do
            errOrToken <-
              testClientOrErr cenv $ do
                NoContent <- clientPostRegister r
                clientLoginSession $ registerLogin r
            case errOrToken of
              Left hp -> expectationFailure $ show hp
              Right _ -> pure ()
