module Smos.Web.Server.Handler.BookingSpec (spec) where

import Smos.Web.Server.Foundation
import Smos.Web.Server.TestUtils
import Test.Syd
import Test.Syd.Yesod

spec :: Spec
spec = do
  smosWebServerSpec $ do
    it "is possible to fetch BookingsR and get a 200 response after loggin in" $ \yc ->
      withAnyFreshAccount_ yc $ do
        get BookingR
        statusIs 200
    it "gets a 404 for BookR for a user that doesn't have booking activated" $ \yc ->
      withAnyFreshAccount yc $ \username _ -> do
        logout
        get $ BookUserR username
        statusIs 404
