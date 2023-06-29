{-# LANGUAGE QuasiQuotes #-}

module Smos.Web.Server.SmosSessionSpec (spec) where

import Control.Monad.Reader
import Path
import Smos.Client
import Smos.Data
import Smos.Web.Server
import Smos.Web.Server.Foundation
import Smos.Web.Server.TestUtils
import Test.Syd
import Test.Syd.Servant
import Test.Syd.Yesod

spec :: Spec
spec = smosWebServerSpec $ do
  it "removes empty directories" $ \yc ->
    withAnyFreshAccount yc $ \username password -> do
      burl <- asks $ appAPIBaseUrl . yesodClientSite
      man <- asks $ appHttpManager . yesodClientSite
      let clientEnv = mkClientEnv man burl
      -- Create foo/bar.smos on the server-side
      NoContent <- liftIO $ runClientOrDie clientEnv $ do
        errOrLogin <- clientLogin Login {loginUsername = username, loginPassword = password}
        case errOrLogin of
          Left err -> liftIO $ expectationFailure $ show err
          Right token -> clientPutSmosFile token [relfile|foo/bar.smos|] emptySmosFile
      get TUIR
      statusIs 200
      pure ()
