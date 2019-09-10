{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Sync.Client.SyncSpec
  ( spec
  ) where

import GHC.Generics (Generic)

import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Mergeful as Mergeful
import qualified Data.Mergeful.Timed as Mergeful
import Data.Set (Set)
import qualified Data.Set as S
import Data.UUID

import Path

import Control.Monad

import Servant.Client

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
    describe "single client" $ do
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
          modifyMaxSize (* 10) $ do
            it "succesfully syncs a list of operations" $ \cenv ->
              forAll genTestOps $ \ops -> do
                initial <- runInitialSync cenv
                let go :: ClientStore -> TestOp -> IO ClientStore
                    go cstore op = do
                      let cstore' = applyTestOpStore cstore op
                      runSync cenv cstore'
                result <- foldM go initial ops
                shouldBeValid result
            it "succesfully syncs a list of phases of operations" $ \cenv ->
              forAll genTestOpsPhases $ \opss -> do
                initial <- runInitialSync cenv
                let go :: ClientStore -> [TestOp] -> IO ClientStore
                    go cstore ops = do
                      let cstore' = applyTestOpsStore cstore ops
                      runSync cenv cstore'
                result <- foldM go initial opss
                shouldBeValid result
    describe "multi client" $ do
      it "succesfully syncs multiple clients with an empty server" $ \cenv ->
        forAllValid $ \units -> do
          stores <-
            forM (units :: [()]) $ \() -> do
              cstore <- runInitialSync cenv
              runSync cenv cstore
          shouldBeValid stores
      modifyMaxSuccess (* 10) $
        modifyMaxSize (* 10) $ do
          it "succesfully syncs a list of operations for clients seperately" $ \cenv ->
            forAll genCTestOps $ \cops -> do
              let go :: Map Int ClientStore -> CTestOp -> IO (Map Int ClientStore)
                  go cstore op = applyCTestOp cenv cstore op
              result <- foldM go M.empty cops
              shouldBeValid result
          it "succesfully syncs a list of phases of operations for clients" $ \cenv ->
            forAll genCTestOpsPhases $ \cops -> do
              let go :: Map Int ClientStore -> [CTestOp] -> IO (Map Int ClientStore)
                  go cstore op = applyCTestOps cenv cstore op
              result <- foldM go M.empty cops
              shouldBeValid result

genCTestOpsPhases :: Gen [[CTestOp]]
genCTestOpsPhases =
  sized $ \n -> do
    part <- arbPartition n
    let go :: (TestSituation, [[CTestOp]]) -> Int -> Gen (TestSituation, [[CTestOp]])
        go (m, os) s = do
          tos <- resize s $ validCTestOpsFor m
          let m' = testApplyCTestOps m tos
          pure (m', tos : os)
    (_, ops) <- foldM go (initialSituation, []) part
    pure $ reverse ops

data CTestOp =
  CTestOp Int ClientOp
  deriving (Show, Eq, Generic)

data ClientOp
  = ClientNew
  | ClientSync
  | ClientTestOps [TestOp]
  deriving (Show, Eq, Generic)

data TestSituation =
  TestSituation
    { testSituationServer :: Set (Path Rel File)
    , testSituationClients :: Map Int (Set (Path Rel File))
    }
  deriving (Show, Eq, Generic)

initialSituation :: TestSituation
initialSituation = TestSituation {testSituationServer = S.empty, testSituationClients = M.empty}

genCTestOps :: Gen [CTestOp]
genCTestOps = validCTestOpsFor initialSituation

validCTestOpsFor :: TestSituation -> Gen [CTestOp]
validCTestOpsFor initial =
  sized $ \n -> do
    part <- arbPartition n
    let go :: (TestSituation, [CTestOp]) -> Int -> Gen (TestSituation, [CTestOp])
        go (m, os) s = do
          to <- resize s $ validCTestOpFor m
          let m' = testApplyCTestOp m to
          pure (m', to : os)
    (_, ops) <- foldM go (initial, []) part
    pure $ reverse ops

validCTestOpFor :: TestSituation -> Gen CTestOp
validCTestOpFor m =
  let addClientGen =
        CTestOp <$> (genValid `suchThat` (\i -> not $ M.member i (testSituationClients m))) <*>
        pure ClientNew
   in frequency $
      (1, addClientGen) :
      if M.null (testSituationClients m)
        then []
        else [ ( 2
               , do (cid, _) <- elements $ M.toList (testSituationClients m)
                    pure $ CTestOp cid ClientSync)
             , ( 5
               , do (cid, cstore) <- elements $ M.toList (testSituationClients m)
                    ops <- validTestOpsFor cstore
                    pure $ CTestOp cid $ ClientTestOps ops)
             ]

testApplyCTestOps :: TestSituation -> [CTestOp] -> TestSituation
testApplyCTestOps = foldl testApplyCTestOp

testApplyCTestOp :: TestSituation -> CTestOp -> TestSituation
testApplyCTestOp ts (CTestOp i cop) =
  let cs = testSituationClients ts
      cs' =
        case cop of
          ClientNew -> M.insert i S.empty cs
          ClientSync -> cs
          ClientTestOps to -> M.adjust (`testApplyTestOps` to) i cs
   in applySync (ts {testSituationClients = cs'}) i

applySync :: TestSituation -> Int -> TestSituation
applySync ts i =
  case M.lookup i (testSituationClients ts) of
    Nothing -> ts
    Just s ->
      ts
        { testSituationClients =
            M.insert i (S.union s (testSituationServer ts)) (testSituationClients ts)
        }

applyCTestOps :: ClientEnv -> Map Int ClientStore -> [CTestOp] -> IO (Map Int ClientStore)
applyCTestOps cenv = foldM (applyCTestOp cenv)

applyCTestOp :: ClientEnv -> Map Int ClientStore -> CTestOp -> IO (Map Int ClientStore)
applyCTestOp cenv m (CTestOp i cop) =
  case cop of
    ClientNew -> do
      initial <- runInitialSync cenv
      pure $ M.insert i initial m
    ClientSync -> do
      case M.lookup i m of
        Nothing -> pure m
        Just cstore -> do
          cstore' <- runSync cenv cstore
          pure $ M.adjust (const cstore') i m
    ClientTestOps ops -> do
      case M.lookup i m of
        Nothing -> pure m
        Just cstore -> do
          let cstore' = applyTestOpsStore cstore ops
          cstore'' <- runSync cenv cstore'
          pure $ M.adjust (const cstore'') i m

data TestOp
  = AddFile SyncFile
  | ChangeFile SyncFile
  | RemoveFile (Path Rel File)
  deriving (Show, Eq, Generic)

genTestOpsPhases :: Gen [[TestOp]]
genTestOpsPhases =
  sized $ \n -> do
    part <- arbPartition n
    let go :: (Set (Path Rel File), [[TestOp]]) -> Int -> Gen (Set (Path Rel File), [[TestOp]])
        go (m, os) s = do
          tos <- resize s $ validTestOpsFor m
          let m' = testApplyTestOps m tos
          pure (m', tos : os)
    (_, ops) <- foldM go (S.empty, []) part
    pure $ reverse ops

genTestOps :: Gen [TestOp]
genTestOps = validTestOpsFor S.empty

validTestOpsFor :: Set (Path Rel File) -> Gen [TestOp]
validTestOpsFor initial =
  sized $ \n -> do
    part <- arbPartition n
    let go :: (Set (Path Rel File), [TestOp]) -> Int -> Gen (Set (Path Rel File), [TestOp])
        go (m, os) s = do
          to <- resize s $ validTestOpFor m
          let m' = testApplyTestOp m to
          pure (m', to : os)
    (_, ops) <- foldM go (initial, []) part
    pure $ reverse ops

validTestOpFor :: Set (Path Rel File) -> Gen TestOp
validTestOpFor s =
  let addFileGen = AddFile <$> (genValid `suchThat` (\sf -> not $ S.member (syncFilePath sf) s))
   in frequency $
      (1, addFileGen) :
      if S.null s
        then []
        else [ ( 3
               , do rf <- elements $ S.toList s
                    v' <- genValid
                    pure $ ChangeFile $ SyncFile {syncFilePath = rf, syncFileContents = v'})
             , ( 1
               , do rf <- elements $ S.elems s
                    pure $ RemoveFile rf)
             ]

testApplyTestOps :: Set (Path Rel File) -> [TestOp] -> Set (Path Rel File)
testApplyTestOps = foldl testApplyTestOp

testApplyTestOp :: Set (Path Rel File) -> TestOp -> Set (Path Rel File)
testApplyTestOp s to =
  case to of
    AddFile SyncFile {..} -> S.insert syncFilePath s
    ChangeFile SyncFile {..} -> S.insert syncFilePath s
    RemoveFile rf -> S.delete rf s

applyTestOpsStore :: ClientStore -> [TestOp] -> ClientStore
applyTestOpsStore = foldl applyTestOpStore

applyTestOpStore :: ClientStore -> TestOp -> ClientStore
applyTestOpStore cstore op =
  let s = clientStoreItems cstore
      s' = applyTestOp s op
   in cstore {clientStoreItems = s'}

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
        Nothing -> Mergeful.addItemToClientStore sf cs
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
