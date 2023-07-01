{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Smos.Web.Server.SmosSessionSpec (spec) where

import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.State
import Data.String
import qualified Data.Text as T
import Data.Time
import Debug.Trace
import Network.HTTP.Client as HTTP
import Network.URI
import qualified Network.WebSockets.Client as WebSocket
import qualified Network.WebSockets.Connection as WebSocket
import Path
import Path.IO
import Smos.Client
import Smos.Data
import Smos.Web.Server.Foundation
import Smos.Web.Server.SmosSession
import Smos.Web.Server.TestUtils
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Yesod
import Text.Read

spec :: Spec
spec = smosWebServerSpec $ do
  it "removes empty directories" $ \yc ->
    forAllValid $ \username ->
      forAll (genValid `suchThat` (not . T.null)) $ \password -> do
        -- We'll need the port for the websocket connection.
        port <- do
          let mPortString = fmap uriPort $ uriAuthority $ yesodClientSiteURI yc
          let stripColumn :: String -> String
              stripColumn = \case
                (':' : s) -> s
                s -> s
          let mPort = mPortString >>= readMaybe . stripColumn
          case mPort of
            Nothing -> expectationFailure "should have gotten a port."
            Just port -> pure port

        let runWebserverSync = do
              cookieJar <- gets yesodClientStateCookies
              lastRequest <- requireRequest
              lastResponse <- requireResponse
              now <- liftIO getCurrentTime
              let host = "0.0.0.0"
              let (cookieHeader, _) = computeCookieString lastRequest cookieJar now False
              liftIO
                $ WebSocket.runClientWith
                  host
                  port
                  "/ws/tui-instance"
                  WebSocket.defaultConnectionOptions
                  [("Set-Cookie", cookieHeader)]
                $ \connection -> do
                  threadDelay 1_000_000

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
          runWebserverSync

        runYesodClientM yc $ do
          burl <- asks $ appAPIBaseUrl . yesodClientSite
          man <- asks $ appHttpManager . yesodClientSite
          let clientEnv = mkClientEnv man burl
          -- Check that the foo/ directory is there
          dataDir <- asks $ appDataDir . yesodClientSite
          perUserDataDir <- liftIO $ resolveUserDataDir dataDir username
          let workflowDir = toWorkflowDir perUserDataDir
          let absFooDir = workflowDir </> [reldir|foo|]

          liftIO $ doesDirExist absFooDir `shouldReturn` True

          -- Remove foo/bar.smos on the server-side
          NoContent <- liftIO $ runClientOrDie clientEnv $ do
            clientDeleteSmosFile token [relfile|foo/bar.smos|]

          -- Here we pretend to start a websocket connection so that the web server will do a sync.
          loginTo username password
          runWebserverSync

          liftIO $ doesDirExist absFooDir `shouldReturn` False
