{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Smos.Sync.Client.Sync
  ( syncSmosSyncClient
  ) where

import GHC.Generics (Generic)

import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.Hashable
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.UUID as UUID (UUID)
import Text.Show.Pretty

import Control.Monad

import System.Exit
import qualified System.FilePath as FP

import Servant.Client

import Path
import Path.IO

import qualified Data.Mergeful as Mergeful
import qualified Data.Mergeful.Timed as Mergeful

import Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS as HTTP

import Smos.Sync.API

import Smos.Sync.Client.OptParse

syncSmosSyncClient :: SyncSettings -> IO ()
syncSmosSyncClient SyncSettings {..} = do
  man <- HTTP.newManager HTTP.tlsManagerSettings
  let cenv = mkClientEnv man syncSetServerUrl
  mMeta <- readStoreMeta syncSetMetadataFile
  meta <-
    case mMeta of
      Nothing
       -- Never synced yet
       --
       -- That means we need to run an initial sync first.
       -> do
        initialStore <- runInitialSync cenv
        pure $ makeClientMetaData initialStore
      Just meta
       -- We have synced before.
       -> pure meta
  files <- readSyncFiles syncSetContentsDir
  let clientStore = consolidateMetaWithFiles meta files
  newClientStore <- runSync cenv clientStore
  saveClientStore syncSetMetadataFile syncSetContentsDir newClientStore

runInitialSync :: ClientEnv -> IO ClientStore
runInitialSync cenv = do
  let clientStore = Mergeful.initialClientStore :: Mergeful.ClientStore UUID SyncFile
  let req = Mergeful.makeSyncRequest clientStore
  pPrint req
  errOrResp <- runClient cenv $ clientSync req
  resp@SyncResponse {..} <-
    case errOrResp of
      Left err -> die $ show err
      Right resp -> pure resp
  pPrint resp
  let items = Mergeful.mergeSyncResponseFromServer Mergeful.initialClientStore syncResponseItems
  pure $ ClientStore {clientStoreServerUUID = syncResponseServerId, clientStoreItems = items}

runSync :: ClientEnv -> ClientStore -> IO ClientStore
runSync cenv clientStore = do
  let items = clientStoreItems clientStore
  let req = Mergeful.makeSyncRequest items
  pPrint req
  errOrResp <- runClient cenv $ clientSync req
  resp@SyncResponse {..} <-
    case errOrResp of
      Left err -> die $ show err
      Right resp -> pure resp
  unless (syncResponseServerId == clientStoreServerUUID clientStore) $
    die $
    unlines
      [ "The server was reset since the last time it was synced with, refusing to sync."
      , "If you want to sync anyway, remove the client metadata file and sync again."
      , "Note that you can lose data by doing this, so make a backup first."
      ]
  pPrint resp
  let newClientStore =
        clientStore
          { clientStoreServerUUID = syncResponseServerId
          , clientStoreItems = Mergeful.mergeSyncResponseIgnoreProblems items syncResponseItems
          }
  pure newClientStore

runClient :: ClientEnv -> ClientM a -> IO (Either ServantError a)
runClient = flip runClientM

clientSync :: SyncRequest -> ClientM SyncResponse
clientSync = client syncAPI

data ClientStore =
  ClientStore
    { clientStoreServerUUID :: UUID
    , clientStoreItems :: Mergeful.ClientStore UUID SyncFile
    }
  deriving (Show, Eq, Generic)

instance FromJSON ClientStore where
  parseJSON = withObject "ClientStore" $ \o -> ClientStore <$> o .: "server-id" <*> o .: "items"

instance ToJSON ClientStore where
  toJSON ClientStore {..} =
    object ["server-id" .= clientStoreServerUUID, "items" .= clientStoreItems]

data ClientMetaData =
  ClientMetaData
    { clientMetaDataServerId :: UUID
    , clientMetaDataMap :: Map (Path Rel File) SyncFileMeta
    }
  deriving (Show, Eq, Generic)

instance FromJSON ClientMetaData where
  parseJSON =
    withObject "ClientMetaData" $ \o -> ClientMetaData <$> o .: "server-id" <*> o .: "items"

instance ToJSON ClientMetaData where
  toJSON ClientMetaData {..} =
    object ["server-id" .= clientMetaDataServerId, "items" .= clientMetaDataMap]

data SyncFileMeta =
  SyncFileMeta
    { syncFileMetaUUID :: UUID
    , syncFileMetaHash :: Int
    , syncFileMetaTime :: Mergeful.ServerTime
    }
  deriving (Show, Eq, Generic)

instance FromJSON SyncFileMeta where
  parseJSON =
    withObject "SyncFileMeta" $ \o -> SyncFileMeta <$> o .: "uuid" <*> o .: "hash" <*> o .: "time"

instance ToJSON SyncFileMeta where
  toJSON SyncFileMeta {..} =
    object ["uuid" .= syncFileMetaUUID, "hash" .= syncFileMetaHash, "time" .= syncFileMetaTime]

readStoreMeta :: Path Abs File -> IO (Maybe ClientMetaData)
readStoreMeta p = do
  mContents <- forgivingAbsence $ LB.readFile $ toFilePath p
  forM mContents $ \contents ->
    case JSON.eitherDecode contents of
      Left err -> die err
      Right store -> pure store

readSyncFiles :: Path Abs Dir -> IO (Map (Path Rel File) ByteString)
readSyncFiles dir = do
  (dirs, files) <- listDirRecur dir
  forM_ dirs ensureDir
  fmap (M.fromList . catMaybes) $
    forM files $ \file ->
      case stripProperPrefix dir file of
        Nothing -> pure Nothing
        Just rfile -> Just <$> ((,) rfile <$> SB.readFile (toFilePath file))

consolidateMetaWithFiles :: ClientMetaData -> Map (Path Rel File) ByteString -> ClientStore
consolidateMetaWithFiles ClientMetaData {..} contentsMap = ClientStore clientMetaDataServerId items
  where
    items
      -- The existing files need to be checked for deletions and changes.
     =
      let go1 ::
               Mergeful.ClientStore UUID SyncFile
            -> Path Rel File
            -> SyncFileMeta
            -> Mergeful.ClientStore UUID SyncFile
          go1 s rf SyncFileMeta {..} =
            case M.lookup rf contentsMap of
              Nothing
               -- The file is not there, that means that it must have been deleted.
               ->
                s
                  { Mergeful.clientStoreDeletedItems =
                      M.insert syncFileMetaUUID syncFileMetaTime $
                      Mergeful.clientStoreDeletedItems s
                  }
              Just contents
               -- The file is there, so we need to check if it has changed.
               -- We will trust hashing. (TODO do we need to fix that?)
               ->
                if hash contents == syncFileMetaHash
                   -- If it hasn't changed, it's still synced.
                  then s
                         { Mergeful.clientStoreSyncedItems =
                             M.insert
                               syncFileMetaUUID
                               (Mergeful.Timed
                                  { Mergeful.timedValue =
                                      SyncFile {syncFilePath = rf, syncFileContents = contents}
                                  , timedTime = syncFileMetaTime
                                  }) $
                             Mergeful.clientStoreSyncedItems s
                         }
                   -- If it has changed, mark it as such
                  else s
                         { Mergeful.clientStoreSyncedButChangedItems =
                             M.insert
                               syncFileMetaUUID
                               (Mergeful.Timed
                                  { Mergeful.timedValue =
                                      SyncFile {syncFilePath = rf, syncFileContents = contents}
                                  , timedTime = syncFileMetaTime
                                  }) $
                             Mergeful.clientStoreSyncedButChangedItems s
                         }
          syncedChangedAndDeleted = M.foldlWithKey go1 Mergeful.initialClientStore clientMetaDataMap
          go2 ::
               Mergeful.ClientStore UUID SyncFile
            -> Path Rel File
            -> ByteString
            -> Mergeful.ClientStore UUID SyncFile
          go2 s rf contents =
            let sf = SyncFile {syncFilePath = rf, syncFileContents = contents}
             in Mergeful.addItemToClientStore sf s
       in M.foldlWithKey go2 syncedChangedAndDeleted (contentsMap `M.difference` clientMetaDataMap)

-- TODO this could be optimised using the sync response
saveClientStore :: Path Abs File -> Path Abs Dir -> ClientStore -> IO ()
saveClientStore metaFile dir store = do
  saveMeta metaFile $ makeClientMetaData store
  saveSyncFiles dir $ clientStoreItems store

-- | We only check the synced items, because it should be the case that
-- they're the only ones that are not empty.
makeClientMetaData :: ClientStore -> ClientMetaData
makeClientMetaData ClientStore {..} =
  let Mergeful.ClientStore {..} = clientStoreItems
   in if not
           (null clientStoreAddedItems &&
            null clientStoreDeletedItems && null clientStoreSyncedButChangedItems)
        then error "Should not happen."
        else let go ::
                      Map (Path Rel File) SyncFileMeta
                   -> UUID
                   -> Mergeful.Timed SyncFile
                   -> Map (Path Rel File) SyncFileMeta
                 go m u Mergeful.Timed {..} =
                   let SyncFile {..} = timedValue
                    in M.insert
                         syncFilePath
                         SyncFileMeta
                           { syncFileMetaUUID = u
                           , syncFileMetaTime = timedTime
                           , syncFileMetaHash = hash syncFileContents
                           }
                         m
              in ClientMetaData clientStoreServerUUID $
                 M.foldlWithKey go M.empty clientStoreSyncedItems

saveMeta :: Path Abs File -> ClientMetaData -> IO ()
saveMeta p store = LB.writeFile (toFilePath p) $ encodePretty store

saveSyncFiles :: Path Abs Dir -> Mergeful.ClientStore UUID SyncFile -> IO ()
saveSyncFiles dir store = do
  tmpDir1 <- resolveDir' $ FP.dropTrailingPathSeparator (toFilePath dir) ++ "-tmp1"
  tmpDir2 <- resolveDir' $ FP.dropTrailingPathSeparator (toFilePath dir) ++ "-tmp2"
  writeAllTo tmpDir1
  renameDir dir tmpDir2
  renameDir tmpDir1 dir
  removeDirRecur tmpDir2
  where
    writeAllTo d = do
      ensureDir d
      void $ M.traverseWithKey go (makeContentsMap store)
      where
        go p bs = do
          let f = d </> p
          ensureDir $ parent f
          SB.writeFile (fromAbsFile f) bs

makeContentsMap :: Mergeful.ClientStore UUID SyncFile -> Map (Path Rel File) ByteString
makeContentsMap Mergeful.ClientStore {..} =
  M.fromList $
  map (\SyncFile {..} -> (syncFilePath, syncFileContents)) $
  concat
    [ M.elems clientStoreAddedItems
    , M.elems $ M.map Mergeful.timedValue clientStoreSyncedItems
    , M.elems $ M.map Mergeful.timedValue clientStoreSyncedButChangedItems
    ]
