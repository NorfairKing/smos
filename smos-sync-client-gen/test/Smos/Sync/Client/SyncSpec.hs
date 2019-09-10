{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Sync.Client.SyncSpec
  ( spec
  ) where

import GHC.Generics (Generic)

import Data.UUID

import Control.Monad

import Test.Hspec
import Test.QuickCheck
import Test.Validity
import Test.Validity.Aeson

import Smos.Sync.API

import Smos.Sync.Server.TestUtils

import Smos.Sync.Client.Sync
import Smos.Sync.Client.Sync.Gen ()

spec :: Spec
spec = do
  genValidSpec @ClientStore
  jsonSpecOnValid @ClientStore
  genValidSpec @ClientMetaData
  jsonSpecOnValid @ClientMetaData
  genValidSpec @SyncFileMeta
  jsonSpecOnValid @SyncFileMeta
  serverSpec $ do
    describe "runInitialSync" $
      it "succesfully gets a valid clientStore from an empty server" $ \cenv -> do
        clientStore <- runInitialSync cenv
        shouldBeValid clientStore
    describe "runSync" $ do
      it "succesfully syncs with an empty server" $ \cenv -> do
        cstore <- runInitialSync cenv
        cstore' <- runSync cenv cstore
        shouldBeValid cstore'
      it "succesfully syncs a list of operations" $ \cenv ->
        forAll genTestOps $ \ops -> do
          initial <- runInitialSync cenv
          let go cstore op = undefined
          result <- foldM go initial ops
          shouldBeValid result

data TestOp
  = AddFile SyncFile
  | ChangeFile UUID SyncFile
  | RemoveFile UUID
  deriving (Show, Eq, Generic)

genTestOps :: Gen [TestOp]
genTestOps = pure []
