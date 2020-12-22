{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Smos.Web.Server.TestUtils where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.Map as M
import Data.Text (Text)
import qualified Network.HTTP.Client as Http
import Network.HTTP.Types
import Path.IO
import Servant.Client
import Smos.Client
import Smos.Data.Gen ()
import qualified Smos.Server.TestUtils as API
import Smos.Web.Server.Application ()
import Smos.Web.Server.Foundation
import Smos.Web.Server.Static
import Test.Syd
import Test.Syd.Yesod
import Yesod.Auth

smosWebServerSpec :: YesodSpec App -> Spec
smosWebServerSpec = API.serverSpec . webServerSpec

webServerSpec :: YesodSpec App -> API.ServerSpec
webServerSpec = setupAroundWith' webServerSetupFunc

webServerSetupFunc :: Http.Manager -> SetupFunc ClientEnv (YesodClient App)
webServerSetupFunc man = connectSetupFunc webServerSetupFunc' (yesodSetupFunc man)

webServerSetupFunc' :: SetupFunc ClientEnv App
webServerSetupFunc' = SetupFunc $ \appFunc (ClientEnv man burl _) ->
  withSystemTempDir "smos-web-server-test-data-dir" $ \tdir -> do
    loginVar <- newTVarIO M.empty
    let app =
          App
            { appLogLevel = LevelWarn,
              appStatic = smosWebServerStatic,
              appAPIBaseUrl = burl,
              appDocsBaseUrl = Nothing,
              appLoginTokens = loginVar,
              appDataDir = tdir,
              appHttpManager = man,
              appGoogleAnalyticsTracking = Nothing,
              appGoogleSearchConsoleVerification = Nothing
            }
    appFunc app

asDummyUser :: YesodExample App a -> YesodExample App a
asDummyUser example_ = do
  let dummyUN = Username "dummy"
      dummyPW = "password"
  withFreshAccount dummyUN dummyPW $ do
    loginTo dummyUN dummyPW
    example_

loginTo :: Username -> Text -> YesodExample App ()
loginTo username passphrase = do
  get $ AuthR LoginR
  statusIs 200
  request $ do
    setMethod methodPost
    setUrl $ AuthR loginFormPostTargetR
    addToken
    addPostParam "user" $ usernameText username
    addPostParam "passphrase" passphrase
  statusIs 303
  loc <- getLocation
  liftIO $ loc `shouldBe` Right HomeR

withFreshAccount ::
  Username -> Text -> YesodExample App a -> YesodExample App a
withFreshAccount exampleUsername examplePassphrase func = do
  get $ AuthR registerR
  statusIs 200
  request $ do
    setMethod methodPost
    setUrl $ AuthR registerR
    addToken
    addPostParam "username" $ usernameText exampleUsername
    addPostParam "passphrase" examplePassphrase
    addPostParam "passphrase-confirm" examplePassphrase
  statusIs 303
  loc <- getLocation
  liftIO $ loc `shouldBe` Right HomeR
  func
