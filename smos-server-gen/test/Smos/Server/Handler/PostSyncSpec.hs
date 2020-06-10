{-# LANGUAGE OverloadedStrings #-}

module Smos.Server.Handler.PostSyncSpec
  ( spec,
  )
where

import qualified Data.Map as M
import qualified Data.Mergeful as Mergeful
import Smos.Client
import Smos.Server.InterestingStore
import Smos.Server.TestUtils
import Test.Hspec
import Test.Validity
import Text.Show.Pretty

spec :: Spec
spec =
  serverSpec $ describe "PostSync" $ do
    it "produces valid resuls" $ \cenv ->
      forAllValid $ \request ->
        withNewUser cenv $ \t -> do
          response <- testClientOrErr cenv (clientPostSync t request)
          shouldBeValid response
    it "produces a valid result after two requests" $ \cenv ->
      forAllValid $ \request1 ->
        forAllValid $ \request2 ->
          withNewUser cenv $ \t -> do
            response <- testClientOrErr cenv $ do
              clientPostSync t request1
              clientPostSync t request2
            shouldBeValid response
    it "produces a valid result after two of the same additions" $ \cenv ->
      forAllValid $ \rp ->
        forAllValid $ \contents1 ->
          forAllValid $ \contents2 ->
            withNewUser cenv $ \t -> do
              let request1 =
                    SyncRequest
                      { syncRequestItems =
                          Mergeful.initialSyncRequest
                            { Mergeful.syncRequestNewItems =
                                M.singleton
                                  rp
                                  ( SyncFile
                                      { syncFilePath = rp,
                                        syncFileContents = contents1
                                      }
                                  )
                            }
                      }
              let request2 =
                    SyncRequest
                      { syncRequestItems =
                          Mergeful.initialSyncRequest
                            { Mergeful.syncRequestNewItems =
                                M.singleton
                                  rp
                                  ( SyncFile
                                      { syncFilePath = rp,
                                        syncFileContents = contents2
                                      }
                                  )
                            }
                      }
              response <- testClientOrErr cenv $ do
                clientPostSync t request1
                clientPostSync t request2
              shouldBeValid response
    xdescribe "Doesn't hold yet."
      $ it "is idempotent after an arbitrary setup request"
      $ \cenv ->
        forAllValid $ \interestingStore ->
          forAllValid $ \request ->
            withNewUser cenv $ \t -> do
              pPrint interestingStore
              print ("Setup" :: String)
              testClientOrErr cenv $ setupInterestingStore t interestingStore
              pPrint request
              print ("request1" :: String)
              response1 <- testClientOrErr cenv (clientPostSync t request)
              pPrint request
              print ("request2" :: String)
              response2 <- testClientOrErr cenv (clientPostSync t request)
              response2 `shouldBe` response1
