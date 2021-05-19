{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.GetPermissionsSpec
  ( spec,
  )
where

import Smos.Client
import Smos.Data.Gen ()
import Smos.Server.TestUtils
import Test.Syd

spec :: Spec
spec =
  describe "GetPermissions" $
    serverSpec $
      it "puts a smos file that can be fetched with GetSmosFile" $ \cenv ->
        withNewUser cenv $ \t -> do
          UserPermissions {..} <- testClient cenv $ clientGetUserPermissions t
          userPermissionsIsAdmin `shouldBe` False
