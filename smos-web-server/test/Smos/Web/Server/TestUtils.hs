module Smos.Web.Server.TestUtils where

import Control.Monad.Logger
import Smos.Web.Server.Application ()
import Smos.Web.Server.Foundation
import Smos.Web.Server.Static
import Test.Hspec
import Yesod.Test

type SmosWebServerSpec = YesodSpec App

type SmosWebServerExample = YesodExample App

smosWebServerSpec :: SmosWebServerSpec -> Spec
smosWebServerSpec = yesodSpecWithSiteGenerator generateSmosWebServer

generateSmosWebServer :: IO App
generateSmosWebServer = do
  let app = App {appLogLevel = LevelDebug, appStatic = smosWebServerStatic}
  pure app
