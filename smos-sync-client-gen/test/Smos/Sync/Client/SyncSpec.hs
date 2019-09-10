{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Sync.Client.SyncSpec
  ( spec
  ) where

import GHC.Generics (Generic)

import Data.ByteString (ByteString)
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Mergeful as Mergeful
import qualified Data.Mergeful.Timed as Mergeful
import Data.UUID

import Path

import Control.Monad

import Test.Hspec
import Test.Hspec.QuickCheck
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
      modifyMaxSuccess (* 10) $
        modifyMaxSize (* 10) $
        it "succesfully syncs a list of operations" $ \cenv ->
          forAll genTestOps $ \ops -> do
            initial <- runInitialSync cenv
            let go :: ClientStore -> TestOp -> IO ClientStore
                go cstore op = do
                  let cstore' =
                        let s = clientStoreItems cstore
                            s' = applyTestOp s op
                         in cstore {clientStoreItems = s'}
                  runSync cenv cstore'
            result <- foldM go initial ops
            shouldBeValid result
      modifyMaxSuccess (* 10) $
        modifyMaxSize (* 10) $
        it "succesfully syncs a u of phases of operations" $ \cenv ->
          forAll genTestOpsPhases $ \opss -> do
            initial <- runInitialSync cenv
            let go :: ClientStore -> [TestOp] -> IO ClientStore
                go cstore ops = do
                  let cstore' =
                        let s = clientStoreItems cstore
                            s' = applyTestOps s ops
                         in cstore {clientStoreItems = s'}
                  runSync cenv cstore'
            result <- foldM go initial opss
            shouldBeValid result

data TestOp
  = AddFile SyncFile
  | ChangeFile SyncFile
  | RemoveFile (Path Rel File)
  deriving (Show, Eq, Generic)

genTestOpsPhases :: Gen [[TestOp]]
genTestOpsPhases = genListOf genTestOps

genTestOps :: Gen [TestOp]
genTestOps =
  sized $ \n -> do
    part <- arbPartition n
    let go ::
             (Map (Path Rel File) ByteString, [TestOp])
          -> Int
          -> Gen (Map (Path Rel File) ByteString, [TestOp])
        go (m, os) s = do
          to <- resize s $ validTestOpFor m
          let m' = testApplyTestOp m to
          pure (m', to : os)
    (_, ops) <- foldM go (M.empty, []) part
    pure $ reverse ops

validTestOpFor :: Map (Path Rel File) ByteString -> Gen TestOp
validTestOpFor m =
  let addFileGen = AddFile <$> (genValid `suchThat` (\sf -> not $ M.member (syncFilePath sf) m))
   in frequency $
      (1, addFileGen) :
      if M.null m
        then []
        else [ ( 2
               , do (rf, v) <- elements $ M.toList m
                    v' <- genValid `suchThat` (/= v)
                    pure $ ChangeFile $ SyncFile {syncFilePath = rf, syncFileContents = v'})
             , ( 1
               , do rf <- elements $ M.keys m
                    pure $ RemoveFile rf)
             ]

testApplyTestOp :: Map (Path Rel File) ByteString -> TestOp -> Map (Path Rel File) ByteString
testApplyTestOp m to =
  case to of
    AddFile SyncFile {..} -> M.insert syncFilePath syncFileContents m
    ChangeFile SyncFile {..} -> M.adjust (const syncFileContents) syncFilePath m
    RemoveFile rf -> M.delete rf m

applyTestOps :: Mergeful.ClientStore UUID SyncFile -> [TestOp] -> Mergeful.ClientStore UUID SyncFile
applyTestOps cs os = foldl applyTestOp cs os

applyTestOp :: Mergeful.ClientStore UUID SyncFile -> TestOp -> Mergeful.ClientStore UUID SyncFile
applyTestOp cs op =
  case op of
    AddFile sf -> Mergeful.addItemToClientStore sf cs
    ChangeFile sf ->
      case find
             (\(_, sf') -> syncFilePath sf == (syncFilePath . Mergeful.timedValue) sf')
             (M.toList $
              M.union
                (Mergeful.clientStoreSyncedItems cs)
                (Mergeful.clientStoreSyncedButChangedItems cs)) of
        Nothing -> cs
        Just (u, _) -> Mergeful.changeItemInClientStore u sf cs
    RemoveFile rf ->
      case find
             (\(_, sf') -> rf == (syncFilePath . Mergeful.timedValue) sf')
             (M.toList $
              M.union
                (Mergeful.clientStoreSyncedItems cs)
                (Mergeful.clientStoreSyncedButChangedItems cs)) of
        Just (u, _) -> Mergeful.markItemDeletedInClientStore u cs
        Nothing ->
          case find
                 (\(_, sf') -> rf == syncFilePath sf')
                 (M.toList $ Mergeful.clientStoreAddedItems cs) of
            Nothing -> cs
            Just (i, _) -> Mergeful.deleteItemFromClientStore i cs
