{-# LANGUAGE OverloadedStrings #-}

module Smos.Server.Handler.PostSyncSpec
  ( spec,
  )
where

import Control.Monad
import qualified Data.Map as M
import qualified Data.Mergeful as Mergeful
import Smos.Client
import Smos.Server.TestUtils
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec =
  serverSpec $
    describe "PostSync" $ do
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
                void $ clientPostSync t request1
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
                                        { syncFileContents = contents1
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
                                        { syncFileContents = contents2
                                        }
                                    )
                              }
                        }
                response <- testClientOrErr cenv $ do
                  void $ clientPostSync t request1
                  clientPostSync t request2
                shouldBeValid response
