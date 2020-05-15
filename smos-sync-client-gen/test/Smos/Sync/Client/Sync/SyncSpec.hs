{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Sync.Client.Sync.SyncSpec
  ( spec,
  )
where

import Control.Monad
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Int
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Mergeful as Mergeful
import qualified Data.Mergeful.Timed as Mergeful
import Data.Set (Set)
import qualified Data.Set as S
import Database.Persist.Sqlite as DB
import GHC.Generics (Generic)
import Path
import Servant.Auth.Client
import Servant.Client
import Smos.API
import Smos.Server.TestUtils
import Smos.Sync.Client.Command.Sync
import Smos.Sync.Client.Env
import Smos.Sync.Client.Sync.Gen ()
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.Validity
import Test.Validity.Aeson

spec :: Spec
spec = do
  genValidSpec @ClientStore
  genValidSpec @SyncFileMeta
  jsonSpecOnValid @SyncFileMeta
  serverSpec $ do
    describe "single client" $ do
      describe "testInitialSync"
        $ it "succesfully gets a valid clientStore from an empty server"
        $ \cenv ->
          withNewUser cenv $ \token -> do
            clientStore <- testInitialSync cenv token
            shouldBeValid clientStore
      describe "testSync" $ do
        it "succesfully syncs with an empty server" $ \cenv ->
          withNewUser cenv $ \token -> do
            cstore <- testInitialSync cenv token
            cstore' <- testSync cenv token cstore
            shouldBeValid cstore'
        modifyMaxSuccess (* 10) $ do
          it "succesfully syncs a list of operations" $ \cenv ->
            forAll genTestOps $ \ops ->
              withNewUser cenv $ \token -> do
                initial <- testInitialSync cenv token
                let go :: ClientStore -> TestOp -> IO ClientStore
                    go cstore op = do
                      let cstore' = applyTestOpStore cstore op
                      testSync cenv token cstore'
                result <- foldM go initial ops
                shouldBeValid result
    describe "multi client" $ do
      it "succesfully syncs multiple clients with an empty server" $ \cenv ->
        forAll (choose (1, 5)) $ \nbClients ->
          withNewUser cenv $ \token -> do
            stores <-
              replicateM nbClients $ do
                cstore <- testInitialSync cenv token
                testSync cenv token cstore
            shouldBeValid stores

data CTestOp
  = CTestOp Int ClientOp
  deriving (Show, Eq, Generic)

data ClientOp
  = ClientNew
  | ClientSync
  | ClientTestOps [TestOp]
  deriving (Show, Eq, Generic)

data TestSituation
  = TestSituation
      { testSituationServer :: Set (Path Rel File),
        testSituationClients :: Map Int (Set (Path Rel File))
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
        CTestOp <$> (genValid `suchThat` (\i -> not $ M.member i (testSituationClients m)))
          <*> pure ClientNew
   in frequency $
        (1, addClientGen)
          : if M.null (testSituationClients m)
            then []
            else
              [ ( 3,
                  do
                    (cid, _) <- elements $ M.toList (testSituationClients m)
                    pure $ CTestOp cid ClientSync
                ),
                ( 5,
                  do
                    (cid, cstore) <- elements $ M.toList (testSituationClients m)
                    ops <- validTestOpsFor cstore
                    pure $ CTestOp cid $ ClientTestOps ops
                )
              ]

testApplyCTestOp :: TestSituation -> CTestOp -> TestSituation
testApplyCTestOp ts (CTestOp i cop) =
  let cs = testSituationClients ts
   in case cop of
        ClientNew -> ts {testSituationClients = M.insert i S.empty cs}
        ClientSync -> applySync ts i
        ClientTestOps to -> ts {testSituationClients = M.adjust (`testApplyTestOps` to) i cs}

applySync :: TestSituation -> Int -> TestSituation
applySync ts i =
  case M.lookup i (testSituationClients ts) of
    Nothing -> ts
    Just s ->
      let u = S.union s (testSituationServer ts)
       in TestSituation
            { testSituationClients = M.insert i u (testSituationClients ts),
              testSituationServer = u
            }

applyCTestOp :: ClientEnv -> Token -> Map Int ClientStore -> CTestOp -> IO (Map Int ClientStore)
applyCTestOp cenv token m (CTestOp i cop) =
  case cop of
    ClientNew -> do
      initial <- testInitialSync cenv token
      pure $ M.insert i initial m
    ClientSync ->
      case M.lookup i m of
        Nothing -> pure m
        Just cstore -> do
          cstore' <- testSync cenv token cstore
          pure $ M.adjust (const cstore') i m
    ClientTestOps ops ->
      case M.lookup i m of
        Nothing -> pure m
        Just cstore -> do
          let cstore' = applyTestOpsStore cstore ops
          pure $ M.adjust (const cstore') i m

testInitialSync :: ClientEnv -> Token -> IO ClientStore
testInitialSync cenv token = testC cenv $ runInitialSync token

testSync :: ClientEnv -> Token -> ClientStore -> IO ClientStore
testSync cenv token cs = testC cenv $ runSync token cs

testC :: ClientEnv -> C a -> IO a
testC cenv func =
  testLogging
    $ DB.withSqlitePool ":memory:" 1
    $ \pool -> do
      let env = SyncClientEnv {syncClientEnvServantClientEnv = cenv, syncClientEnvConnection = pool}
      runReaderT func env

testLogging :: LoggingT IO a -> IO a
testLogging = runStderrLoggingT . filterLogger (\_ ll -> ll >= LevelWarn)

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
        (1, addFileGen)
          : if S.null s
            then []
            else
              [ ( 3,
                  do
                    rf <- elements $ S.toList s
                    v' <- genValid
                    pure $ ChangeFile $ SyncFile {syncFilePath = rf, syncFileContents = v'}
                ),
                ( 1,
                  do
                    rf <- elements $ S.elems s
                    pure $ RemoveFile rf
                )
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

applyTestOp ::
  Mergeful.ClientStore Int64 FileUUID SyncFile -> TestOp -> Mergeful.ClientStore Int64 FileUUID SyncFile
applyTestOp cs op =
  case op of
    AddFile sf -> Mergeful.addItemToClientStore sf cs
    ChangeFile sf ->
      case find
        (\(_, sf') -> syncFilePath sf == (syncFilePath . Mergeful.timedValue) sf')
        ( M.toList $
            M.union
              (Mergeful.clientStoreSyncedItems cs)
              (Mergeful.clientStoreSyncedButChangedItems cs)
        ) of
        Nothing -> Mergeful.addItemToClientStore sf cs
        Just (u, _) -> Mergeful.changeItemInClientStore u sf cs
    RemoveFile rf ->
      case find
        (\(_, sf') -> rf == (syncFilePath . Mergeful.timedValue) sf')
        ( M.toList $
            M.union
              (Mergeful.clientStoreSyncedItems cs)
              (Mergeful.clientStoreSyncedButChangedItems cs)
        ) of
        Just (u, _) -> Mergeful.markItemDeletedInClientStore u cs
        Nothing ->
          case find
            (\(_, sf') -> rf == syncFilePath sf')
            (M.toList $ Mergeful.clientStoreAddedItems cs) of
            Nothing -> cs
            Just (i, _) -> Mergeful.deleteItemFromClientStore i cs
