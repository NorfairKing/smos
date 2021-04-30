{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Smos.Web.Server.TestUtils where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
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
import Smos.Web.Style
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Yesod
import Yesod.Auth

smosWebServerSpec :: YesodSpec App -> Spec
smosWebServerSpec = API.serverSpec . webServerSpec

webServerSpec :: YesodSpec App -> API.ServerSpec
webServerSpec = setupAroundWith' webServerSetupFunc

webServerSetupFunc :: Http.Manager -> SetupFunc ClientEnv (YesodClient App)
webServerSetupFunc man = connectSetupFunc webServerSetupFunc' (yesodClientSetupFunc man)

webServerSetupFunc' :: SetupFunc ClientEnv App
webServerSetupFunc' = wrapSetupFunc $ \(ClientEnv man burl _) -> do
  tdir <- makeSimpleSetupFunc $ withSystemTempDir "smos-web-server-test-data-dir"
  loginVar <- liftIO $ newTVarIO M.empty
  pure
    App
      { appLogLevel = LevelWarn,
        appStatic = smosWebServerStatic,
        appStyle = smosWebStyle,
        appAPIBaseUrl = burl,
        appDocsBaseUrl = Nothing,
        appLoginTokens = loginVar,
        appDataDir = tdir,
        appHttpManager = man,
        appGoogleAnalyticsTracking = Nothing,
        appGoogleSearchConsoleVerification = Nothing
      }

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

withAnyFreshAccount_ :: YesodClient App -> YesodClientM App () -> Property
withAnyFreshAccount_ yc func = withAnyFreshAccount yc (\_ _ -> func)

withAnyFreshAccount :: YesodClient App -> (Username -> Text -> YesodClientM App ()) -> Property
withAnyFreshAccount yc func = forAllValid $ \username ->
  forAll (genValid `suchThat` (not . T.null)) $ \password -> do
    runYesodClientM yc $ do
      registerAccount username password
      func username password

registerAccount ::
  Username -> Text -> YesodExample App ()
registerAccount exampleUsername examplePassphrase = do
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
