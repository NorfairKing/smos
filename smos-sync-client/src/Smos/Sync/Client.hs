{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Smos.Sync.Client
  ( smosSyncClient
  ) where

import Data.Aeson as JSON
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.UUID as UUID

import Control.Monad

import System.Environment
import System.Exit
import qualified System.FilePath as FP

import Servant.Client

import Path
import Path.IO

import Data.Mergeful

import Network.HTTP.Client as HTTP

import Smos.Sync.Server

smosSyncClient :: IO ()
smosSyncClient = do
  (metaFileFP:dirFP:_) <- getArgs
  metaFile <- resolveFile' metaFileFP
  dir <- resolveDir' dirFP
  clientStore <- readClientStore metaFile dir
  man <- HTTP.newManager HTTP.defaultManagerSettings
  burl <- parseBaseUrl "localhost:8000"
  let cenv = mkClientEnv man burl
  errOrResp <- runClient cenv $ clientSync $ makeSyncRequest clientStore
  resp <-
    case errOrResp of
      Left err -> die $ show err
      Right resp -> pure resp
  saveClientStore metaFile dir $ mergeSyncResponseIgnoreProblems clientStore resp

runClient :: ClientEnv -> ClientM a -> IO (Either ServantError a)
runClient = flip runClientM

clientSync :: SyncRequest UUID SyncFile -> ClientM (SyncResponse UUID SyncFile)
clientSync = client syncAPI

readClientStore :: Path Abs File -> Path Abs Dir -> IO (ClientStore UUID SyncFile)
readClientStore metaFile dir = do
  meta <- readStoreMeta metaFile
  files <- readSyncFiles dir
  pure $ consolidateMetaWithFiles meta files

-- TODO make the metadata different from the store.
-- there are a lot of possible efficiency gains.
-- We could use hashes to speed things up, for example, I think.
readStoreMeta :: Path Abs File -> IO (ClientStore UUID SyncFile)
readStoreMeta p = do
  mContents <- forgivingAbsence $ LB.readFile $ toFilePath p
  case mContents of
    Nothing -> pure emptyClientStore
    Just contents ->
      case JSON.eitherDecode contents of
        Left err -> die err
        Right store -> pure store

readSyncFiles :: Path Abs Dir -> IO (Map (Path Rel File) ByteString)
readSyncFiles dir = do
  files <- snd <$> listDirRecur dir
  fmap (M.fromList . catMaybes) $
    forM files $ \file ->
      case stripProperPrefix dir file of
        Nothing -> pure Nothing
        Just rfile -> Just <$> ((,) rfile <$> SB.readFile (toFilePath file))

consolidateMetaWithFiles ::
     ClientStore UUID SyncFile -> Map (Path Rel File) ByteString -> ClientStore UUID SyncFile
consolidateMetaWithFiles cs contentsMap
    -- We leave the existing added, the changed and the deleted items as is.
    -- There shouldn't be any of those anyway.
    -- So we only need to go through the synced items
    -- to see if anything has been changed or deleted.
    -- Afterwards we also go through the rest of the items.
    -- Those are the new added ones
 =
  let go :: ClientStore UUID SyncFile -> UUID -> Timed SyncFile -> ClientStore UUID SyncFile
      go s u t@Timed {..} =
        case M.lookup (syncFilePath timedValue) contentsMap of
          Nothing
          -- The file is not there, that means that it must have been deleted.
           -> s {clientStoreDeletedItems = M.insert u timedTime $ clientStoreDeletedItems s}
          Just contents
          -- The file is there, so we need to check if it has changed.
           ->
            if contents == syncFileContents timedValue
              -- If it hasn't changed, it's still synced.
              then s {clientStoreSyncedItems = M.insert u t $ clientStoreSyncedItems s}
              -- If it has changed, mark it as such
              else s
                     { clientStoreSyncedButChangedItems =
                         M.insert u (t { timedValue = timedValue { syncFileContents = contents}} ) $ clientStoreSyncedButChangedItems s
                     }
      checked =
        M.foldlWithKey go (cs {clientStoreSyncedItems = M.empty}) (clientStoreSyncedItems cs)
      added = map (uncurry SyncFile) $ M.toList $ contentsMap `M.difference` makeContentsMap cs
   in checked {clientStoreAddedItems = nub $ added ++ clientStoreAddedItems checked}

-- TODO this could be optimised using the sync response
saveClientStore :: Path Abs File -> Path Abs Dir -> ClientStore UUID SyncFile -> IO ()
saveClientStore metaFile dir store = do
  saveMeta metaFile store
  saveSyncFiles dir store

saveMeta :: Path Abs File -> ClientStore UUID SyncFile -> IO ()
saveMeta p store = encodeFile (toFilePath p) store

saveSyncFiles :: Path Abs Dir -> ClientStore UUID SyncFile -> IO ()
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
        go p bs = SB.writeFile (fromAbsFile (d </> p)) bs

makeContentsMap :: ClientStore UUID SyncFile -> Map (Path Rel File) ByteString
makeContentsMap ClientStore {..} =
  M.fromList $
  map (\SyncFile {..} -> (syncFilePath, syncFileContents)) $
  concat
    [ clientStoreAddedItems
    , M.elems $ M.map timedValue clientStoreSyncedItems
    , M.elems $ M.map timedValue clientStoreSyncedButChangedItems
    ]
