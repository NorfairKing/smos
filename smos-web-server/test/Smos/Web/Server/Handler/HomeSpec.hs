module Smos.Web.Server.Handler.HomeSpec (spec) where

import Smos.Web.Server.Foundation
import Smos.Web.Server.TestUtils
import Test.Syd
import Yesod.Test

spec :: Spec
spec = smosWebServerSpec $ do
  yit "is possible to fetch HomeR and get a 200 response" $ do
    get HomeR
    statusIs 200
