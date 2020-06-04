module Smos.Web.Server.Handler.NextSpec (spec) where

import Smos.Web.Server.Foundation
import Smos.Web.Server.TestUtils
import Test.Hspec
import Yesod.Test

spec :: Spec
spec = smosWebServerSpec $ do
  yit "is possible to fetch NextR and get a 200 response" $ asDummyUser $ do
    get ReportNextR
    statusIs 200
