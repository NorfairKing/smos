{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Smos.Web.Server.SmosSessionSpec (spec) where

import Control.Monad.Reader
import qualified Data.Text as T
import Path
import Path.IO
import Smos.Client
import Smos.Data
import Smos.Web.Server
import Smos.Web.Server.Foundation
import Smos.Web.Server.SmosSession
import Smos.Web.Server.TestUtils
import Test.QuickCheck
import Test.Syd
import Test.Syd.Servant
import Test.Syd.Validity
import Test.Syd.Yesod

spec :: Spec
spec = smosWebServerSpec $ do
  it "removes empty directories" $ \yc ->
    forAllValid $ \username ->
      forAll (genValid `suchThat` (not . T.null)) $ \password -> do
        -- We have to run 'runYesodClientM' separately because we need separate cookies for the synk requests
        token <- runYesodClientM yc $ do
          registerAccount username password

          burl <- asks $ appAPIBaseUrl . yesodClientSite
          man <- asks $ appHttpManager . yesodClientSite
          let clientEnv = mkClientEnv man burl

          -- Create foo/bar.smos on the server-side
          liftIO $ runClientOrDie clientEnv $ do
            errOrLogin <- clientLogin Login {loginUsername = username, loginPassword = password}
            case errOrLogin of
              Left err -> liftIO $ expectationFailure $ show err
              Right token -> do
                NoContent <- clientPutSmosFile token [relfile|foo/bar.smos|] emptySmosFile
                pure token

        runYesodClientM yc $ do
          loginTo username password
          -- Here we pretend to start a websocket connection so that the web server will do a sync.
          request $ do
            setUrl TUIInstanceR
            setMethod methodGet
            addRequestHeader ("Upgrade", "websocket")
            addRequestHeader ("Connection", "upgrade")
            addRequestHeader ("Sec-WebSocket-Key", "x3JJHMbDL1EzLkh9GBhXDw==")
            addRequestHeader ("Sec-WebSocket-Version", "13")
          statusIs 101

        runYesodClientM yc $ do
          burl <- asks $ appAPIBaseUrl . yesodClientSite
          man <- asks $ appHttpManager . yesodClientSite
          let clientEnv = mkClientEnv man burl
          -- Check that the foo/ directory is there
          dataDir <- asks $ appDataDir . yesodClientSite
          userDataDir <- liftIO $ resolveUserDataDir dataDir username
          let workflowDir = toWorkflowDir userDataDir
          let absFooDir = workflowDir </> [reldir|foo|]

          liftIO $ doesDirExist absFooDir `shouldReturn` True

          -- Remove foo/bar.smos on the server-side
          NoContent <- liftIO $ runClientOrDie clientEnv $ do
            clientDeleteSmosFile token [relfile|foo/bar.smos|]

          -- Here we pretend to start a websocket connection so that the web server will do a sync.
          loginTo username password
          request $ do
            setUrl TUIInstanceR
            setMethod methodGet
            addRequestHeader ("Upgrade", "websocket")
            addRequestHeader ("Connection", "upgrade")
            addRequestHeader ("Sec-WebSocket-Key", "x3JJHMbDL1EzLkh9GBhXDw==")
            addRequestHeader ("Sec-WebSocket-Version", "13")
          statusIs 101

          liftIO $ doesDirExist absFooDir `shouldReturn` False
