{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Smos.Sync.Client.Sync where

import GHC.Generics (Generic)

import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.Hashable
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.UUID as UUID (UUID)
import Data.Validity
import Data.Validity.UUID ()
import Text.Show.Pretty

import Control.Monad
import Control.Monad.Logger

import System.Exit

import Servant.Client

import Path
import Path.IO

import qualified Data.Mergeful as Mergeful
import qualified Data.Mergeful.Timed as Mergeful

import Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS as HTTP

import Conduit

import Smos.Sync.API

import Smos.Sync.Client.Contents
import Smos.Sync.Client.ContentsMap (ContentsMap(..))
import Smos.Sync.Client.OptParse
import Smos.Sync.Client.OptParse.Types

syncSmosSyncClient :: Settings -> SyncSettings -> IO ()
syncSmosSyncClient Settings {..} SyncSettings {..} =
  runStderrLoggingT $
  filterLogger (\_ ll -> ll >= setLogLevel) $ do
    logDebugN "CLIENT START"
    man <- liftIO $ HTTP.newManager HTTP.tlsManagerSettings
    let cenv = mkClientEnv man syncSetServerUrl
    mMeta <- liftIO $ readStoreMeta syncSetMetadataFile
    logDebugData "READ STORED METADATA" mMeta
    files <- liftIO $ readFilteredSyncFiles syncSetIgnoreFiles syncSetContentsDir
    logDebugData "READ FILE CONTENTS" files
    clientStore <-
      case mMeta of
        Nothing
       -- Never synced yet
       --
       -- That means we need to run an initial sync first.
         -> do
          initialStore <- runInitialSync cenv
          pure $ consolidateInitialStoreWithFiles initialStore files
        Just meta
       -- We have synced before.
         -> pure $ consolidateMetaWithFiles meta files
    logDebugData "CLIENT STORE BEFORE SYNC" clientStore
    newClientStore <- runSync cenv clientStore
    logDebugData "CLIENT STORE AFTER SYNC" newClientStore
    liftIO $
      saveClientStore syncSetIgnoreFiles syncSetMetadataFile syncSetContentsDir newClientStore
    logDebugN "CLIENT END"

type C = LoggingT IO

runInitialSync :: ClientEnv -> C ClientStore
runInitialSync cenv = do
  logDebugN "INITIAL SYNC START"
  let clientStore = Mergeful.initialClientStore :: Mergeful.ClientStore UUID SyncFile
  let req = Mergeful.makeSyncRequest clientStore
  logDebugData "INITIAL SYNC REQUEST" req
  logInfoJsonData "INITIAL SYNC REQUEST (JSON)" req
  resp@SyncResponse {..} <- liftIO $ runClientOrDie cenv $ clientSync req
  logDebugData "INITIAL SYNC RESPONSE" resp
  logInfoJsonData "INITIAL SYNC RESPONSE (JSON)" resp
  let items = Mergeful.mergeSyncResponseFromServer Mergeful.initialClientStore syncResponseItems
  let newClientStore =
        ClientStore {clientStoreServerUUID = syncResponseServerId, clientStoreItems = items}
  logDebugData "INITIAL CLIENT STORE" newClientStore
  logDebugN "INITIAL SYNC END"
  pure newClientStore

runSync :: ClientEnv -> ClientStore -> C ClientStore
runSync cenv clientStore = do
  logDebugN "SYNC START"
  let items = clientStoreItems clientStore
  let req = Mergeful.makeSyncRequest items
  logDebugData "SYNC REQUEST" req
  logInfoJsonData "SYNC REQUEST (JSON)" req
  resp@SyncResponse {..} <- liftIO $ runClientOrDie cenv $ clientSync req
  logDebugData "SYNC RESPONSE" resp
  logInfoJsonData "SYNC RESPONSE (JSON)" resp
  liftIO $
    unless (syncResponseServerId == clientStoreServerUUID clientStore) $
    die $
    unlines
      [ "The server was reset since the last time it was synced with, refusing to sync."
      , "If you want to sync anyway, remove the client metadata file and sync again."
      , "Note that you can lose data by doing this, so make a backup first."
      ]
  let newClientStore =
        clientStore
          { clientStoreServerUUID = syncResponseServerId
          , clientStoreItems = Mergeful.mergeSyncResponseFromServer items syncResponseItems
          }
  logDebugN "SYNC END"
  pure newClientStore

logInfoJsonData :: ToJSON a => Text -> a -> C ()
logInfoJsonData name a =
  logInfoN $ T.unwords [name <> ":", TE.decodeUtf8 $ LB.toStrict $ encodePretty a]

logDebugData :: Show a => Text -> a -> C ()
logDebugData name a = logDebugN $ T.unwords [name <> ":", T.pack $ ppShow a]

runClient :: ClientEnv -> ClientM a -> IO (Either ClientError a)
runClient = flip runClientM

runClientOrDie :: ClientEnv -> ClientM a -> IO a
runClientOrDie cenv func = do
  errOrResp <- runClient cenv func
  case errOrResp of
    Left err -> die $ show err
    Right resp -> pure resp

clientSync :: SyncRequest -> ClientM SyncResponse
clientSync = client syncAPI

data ClientStore =
  ClientStore
    { clientStoreServerUUID :: UUID
    , clientStoreItems :: Mergeful.ClientStore UUID SyncFile
    }
  deriving (Show, Eq, Generic)

instance Validity ClientStore

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

instance Validity ClientMetaData

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

instance Validity SyncFileMeta

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

consolidateInitialStoreWithFiles :: ClientStore -> ContentsMap -> ClientStore
consolidateInitialStoreWithFiles cs contentsMap =
  let Mergeful.ClientStore {..} = clientStoreItems cs
   in if not
           (null clientStoreAddedItems &&
            null clientStoreDeletedItems && null clientStoreSyncedButChangedItems)
        then error "should not happen: initial"
        else cs
               { clientStoreItems =
                   consolidateInitialSyncedItemsWithFiles clientStoreSyncedItems contentsMap
               }

consolidateInitialSyncedItemsWithFiles ::
     Map UUID (Mergeful.Timed SyncFile) -> ContentsMap -> Mergeful.ClientStore UUID SyncFile
consolidateInitialSyncedItemsWithFiles syncedItems =
  M.foldlWithKey go (Mergeful.initialClientStore {Mergeful.clientStoreSyncedItems = syncedItems}) .
  contentsMapFiles
  where
    alreadySyncedMap = makeAlreadySyncedMap syncedItems
    go ::
         Mergeful.ClientStore UUID SyncFile
      -> Path Rel File
      -> ByteString
      -> Mergeful.ClientStore UUID SyncFile
    go s rf contents =
      let sf = SyncFile {syncFileContents = contents, syncFilePath = rf}
       in case M.lookup rf alreadySyncedMap of
            Nothing
          -- Not in the initial sync, that means it was added
             -> Mergeful.addItemToClientStore sf s
            Just (i, contents') ->
              if contents == contents'
                -- We the same file locally, do nothing.
                then s
                -- We have a different file locally, so we'll mark this as 'synced but changed'.
                else Mergeful.changeItemInClientStore i sf s

makeAlreadySyncedMap :: Map i (Mergeful.Timed SyncFile) -> Map (Path Rel File) (i, ByteString)
makeAlreadySyncedMap m = M.fromList $ map go $ M.toList m
  where
    go (i, Mergeful.Timed SyncFile {..} _) = (syncFilePath, (i, syncFileContents))

consolidateMetaWithFiles :: ClientMetaData -> ContentsMap -> ClientStore
consolidateMetaWithFiles ClientMetaData {..} contentsMap =
  ClientStore clientMetaDataServerId $ consolidateMetaMapWithFiles clientMetaDataMap contentsMap

consolidateMetaMapWithFiles ::
     Map (Path Rel File) SyncFileMeta -> ContentsMap -> Mergeful.ClientStore UUID SyncFile
consolidateMetaMapWithFiles clientMetaDataMap contentsMap
      -- The existing files need to be checked for deletions and changes.
 =
  let go1 ::
           Mergeful.ClientStore UUID SyncFile
        -> Path Rel File
        -> SyncFileMeta
        -> Mergeful.ClientStore UUID SyncFile
      go1 s rf sfm@SyncFileMeta {..} =
        case M.lookup rf $ contentsMapFiles contentsMap of
          Nothing
               -- The file is not there, that means that it must have been deleted.
               -- so we will mark it as such
           ->
            s
              { Mergeful.clientStoreDeletedItems =
                  M.insert syncFileMetaUUID syncFileMetaTime $ Mergeful.clientStoreDeletedItems s
              }
          Just contents
               -- The file is there, so we need to check if it has changed.
           ->
            if isUnchanged sfm contents
                   -- If it hasn't changed, it's still synced.
              then s
                     { Mergeful.clientStoreSyncedItems =
                         M.insert
                           syncFileMetaUUID
                           (Mergeful.Timed
                              { Mergeful.timedValue =
                                  SyncFile {syncFilePath = rf, syncFileContents = contents}
                              , timedTime = syncFileMetaTime
                              })
                           (Mergeful.clientStoreSyncedItems s)
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
                              })
                           (Mergeful.clientStoreSyncedButChangedItems s)
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
   in M.foldlWithKey
        go2
        syncedChangedAndDeleted
        (contentsMapFiles contentsMap `M.difference` clientMetaDataMap)

-- We will trust hashing. (TODO do we need to fix that?)
isUnchanged :: SyncFileMeta -> ByteString -> Bool
isUnchanged SyncFileMeta {..} contents = hash contents == syncFileMetaHash

-- TODO this could be optimised using the sync response
saveClientStore :: IgnoreFiles -> Path Abs File -> Path Abs Dir -> ClientStore -> IO ()
saveClientStore igf metaFile dir store = do
  saveMeta metaFile $ makeClientMetaData igf store
  saveSyncFiles igf dir $ clientStoreItems store

-- | We only check the synced items, because it should be the case that
-- they're the only ones that are not empty.
makeClientMetaData :: IgnoreFiles -> ClientStore -> ClientMetaData
makeClientMetaData igf ClientStore {..} =
  let Mergeful.ClientStore {..} = clientStoreItems
   in if not
           (null clientStoreAddedItems &&
            null clientStoreDeletedItems && null clientStoreSyncedButChangedItems)
        then error "Should not happen: make meta"
        else let go ::
                      Map (Path Rel File) SyncFileMeta
                   -> UUID
                   -> Mergeful.Timed SyncFile
                   -> Map (Path Rel File) SyncFileMeta
                 go m u Mergeful.Timed {..} =
                   let SyncFile {..} = timedValue
                       goOn =
                         M.insert
                           syncFilePath
                           SyncFileMeta
                             { syncFileMetaUUID = u
                             , syncFileMetaTime = timedTime
                             , syncFileMetaHash = hash syncFileContents
                             }
                           m
                    in case igf of
                         IgnoreNothing -> goOn
                         IgnoreHiddenFiles ->
                           if isHidden syncFilePath
                             then m
                             else goOn
              in ClientMetaData clientStoreServerUUID $
                 M.foldlWithKey go M.empty clientStoreSyncedItems

saveMeta :: Path Abs File -> ClientMetaData -> IO ()
saveMeta p store = LB.writeFile (toFilePath p) $ encodePretty store

saveSyncFiles :: IgnoreFiles -> Path Abs Dir -> Mergeful.ClientStore UUID SyncFile -> IO ()
saveSyncFiles igf dir store = saveContentsMap igf dir $ makeContentsMap store
