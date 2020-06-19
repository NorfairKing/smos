{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Smos.Sync.Client.Command.Sync
  ( syncSmosSyncClient,
  )
where

import Control.Monad
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Mergeful as Mergeful
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.Validity.UUID ()
import Database.Persist.Sqlite as DB
import Pantry.SHA256 as SHA256
import Path
import Path.IO
import Smos.Client
import Smos.Sync.Client.Contents
import Smos.Sync.Client.ContentsMap (ContentsMap (..))
import Smos.Sync.Client.DB
import Smos.Sync.Client.Env
import Smos.Sync.Client.Meta
import Smos.Sync.Client.MetaMap (MetaMap (..))
import Smos.Sync.Client.OptParse
import System.Exit
import System.FileLock
import Text.Show.Pretty

syncSmosSyncClient :: Settings -> SyncSettings -> IO ()
syncSmosSyncClient Settings {..} SyncSettings {..} = do
  ensureDir $ parent syncSetMetadataDB
  withFileLock (fromAbsFile syncSetMetadataDB) Exclusive $ \_ ->
    runStderrLoggingT
      $ filterLogger (\_ ll -> ll >= setLogLevel)
      $ DB.withSqlitePool (T.pack $ fromAbsFile syncSetMetadataDB) 1
      $ \pool ->
        withClientEnv setServerUrl $ \cenv ->
          withLogin cenv setSessionPath setUsername setPassword $ \token -> do
            logDebugN "CLIENT START"
            let env =
                  SyncClientEnv {syncClientEnvServantClientEnv = cenv, syncClientEnvConnection = pool}
            flip runReaderT env $ do
              void $ runDB $ runMigrationSilent migrateAll
              mUUID <- liftIO $ readServerUUID syncSetUUIDFile
              serverUUID <- case mUUID of
                -- Never synced before
                Nothing -> do
                  serverUUID <- runInitialSync syncSetContentsDir syncSetIgnoreFiles token
                  liftIO $ writeServerUUID syncSetUUIDFile serverUUID
                  pure serverUUID
                -- Already synced before
                Just serverUUID -> pure serverUUID
              runSync syncSetContentsDir syncSetBackupDir syncSetIgnoreFiles serverUUID token
            logDebugN "CLIENT END"

runInitialSync :: Path Abs Dir -> IgnoreFiles -> Token -> C ServerUUID
runInitialSync contentsDir ignoreFiles token = do
  logDebugN "INITIAL SYNC START"
  let req = SyncRequest {syncRequestItems = Mergeful.initialSyncRequest :: Mergeful.SyncRequest (Path Rel File) (Path Rel File) SyncFile}
  logDebugData "INITIAL SYNC REQUEST" req
  logInfoJsonData "INITIAL SYNC REQUEST (JSON)" req
  resp@SyncResponse {..} <- runSyncClientOrDie $ clientPostSync token req
  logDebugData "INITIAL SYNC RESPONSE" resp
  logInfoJsonData "INITIAL SYNC RESPONSE (JSON)" resp
  clientMergeInitialSyncResponse contentsDir ignoreFiles syncResponseItems
  logDebugN "INITIAL SYNC END"
  pure syncResponseServerId

runSync :: Path Abs Dir -> Path Abs Dir -> IgnoreFiles -> ServerUUID -> Token -> C ()
runSync contentsDir backupDir ignoreFiles serverUUID token = do
  logDebugN "SYNC START"
  req <- clientMakeSyncRequest contentsDir ignoreFiles
  logDebugData "SYNC REQUEST" req
  logInfoJsonData "SYNC REQUEST (JSON)" req
  resp@SyncResponse {..} <- runSyncClientOrDie $ clientPostSync token req
  logDebugData "SYNC RESPONSE" resp
  logInfoJsonData "SYNC RESPONSE (JSON)" resp
  liftIO
    $ unless (syncResponseServerId == serverUUID)
    $ die
    $ unlines
      [ "The server was reset since the last time it was synced with, refusing to sync.",
        "If you want to sync anyway, remove the client metadata file and sync again.",
        "Note that you can lose data by doing this, so make a backup first."
      ]
  clientMergeSyncResponse contentsDir backupDir ignoreFiles syncResponseItems
  logDebugN "SYNC END"

clientMakeSyncRequest :: Path Abs Dir -> IgnoreFiles -> C SyncRequest
clientMakeSyncRequest contentsDir ignoreFiles = do
  files <- liftIO $ readFilteredSyncFiles ignoreFiles contentsDir
  logDebugData "CLIENT CONTENTS MAP BEFORE SYNC" files
  meta <- runDB readClientMetadata
  logDebugData "CLIENT META MAP BEFORE SYNC" meta
  let syncRequestItems = consolidateToSyncRequest meta files
  pure SyncRequest {..}

consolidateToSyncRequest :: MetaMap -> ContentsMap -> Mergeful.SyncRequest (Path Rel File) (Path Rel File) SyncFile
consolidateToSyncRequest clientMetaDataMap contentsMap =
  -- The existing files need to be checked for deletions and changes.
  let go1 ::
        Mergeful.SyncRequest (Path Rel File) (Path Rel File) SyncFile ->
        Path Rel File ->
        SyncFileMeta ->
        Mergeful.SyncRequest (Path Rel File) (Path Rel File) SyncFile
      go1 s rf sfm@SyncFileMeta {..} =
        case M.lookup rf $ contentsMapFiles contentsMap of
          Nothing ->
            -- The file is not there, that means that it must have been deleted.
            -- so we will mark it as such
            s
              { Mergeful.syncRequestDeletedItems =
                  M.insert rf syncFileMetaTime $ Mergeful.syncRequestDeletedItems s
              }
          Just contents ->
            -- The file is there, so we need to check if it has changed.
            if isUnchanged sfm contents
              then-- If it hasn't changed, it's still synced.

                s
                  { Mergeful.syncRequestKnownItems =
                      M.insert
                        rf
                        syncFileMetaTime
                        (Mergeful.syncRequestKnownItems s)
                  }
              else-- If it has changed, mark it as such

                s
                  { Mergeful.syncRequestKnownButChangedItems =
                      M.insert
                        rf
                        ( Mergeful.Timed
                            { Mergeful.timedValue =
                                SyncFile {syncFileContents = contents},
                              timedTime = syncFileMetaTime
                            }
                        )
                        (Mergeful.syncRequestKnownButChangedItems s)
                  }
      syncedChangedAndDeleted =
        M.foldlWithKey go1 Mergeful.initialSyncRequest $ metaMapFiles clientMetaDataMap
      go2 ::
        Mergeful.SyncRequest (Path Rel File) (Path Rel File) SyncFile ->
        Path Rel File ->
        ByteString ->
        Mergeful.SyncRequest (Path Rel File) (Path Rel File) SyncFile
      go2 s rf contents =
        let sf = SyncFile {syncFileContents = contents}
         in s {Mergeful.syncRequestNewItems = M.insert rf sf $ Mergeful.syncRequestNewItems s}
   in M.foldlWithKey
        go2
        syncedChangedAndDeleted
        (contentsMapFiles contentsMap `M.difference` metaMapFiles clientMetaDataMap)

clientMergeInitialSyncResponse :: Path Abs Dir -> IgnoreFiles -> Mergeful.SyncResponse (Path Rel File) (Path Rel File) SyncFile -> C ()
clientMergeInitialSyncResponse contentsDir ignoreFiles Mergeful.SyncResponse {..} = do
  unless
    ( and
        [ null syncResponseClientAdded,
          null syncResponseClientChanged,
          null syncResponseClientDeleted,
          null syncResponseServerChanged,
          null syncResponseServerDeleted,
          null syncResponseConflicts,
          null syncResponseConflictsClientDeleted,
          null syncResponseConflictsServerDeleted
        ]
    )
    $ liftIO
    $ die "Something serious went wrong during the first sync: somehow there is already something other than clean downloading happening."
  clientMergeInitialServerAdditions contentsDir ignoreFiles syncResponseServerAdded

clientMergeInitialServerAdditions ::
  Path Abs Dir ->
  IgnoreFiles ->
  Map (Path Rel File) (Mergeful.Timed SyncFile) ->
  C ()
clientMergeInitialServerAdditions contentsDir ignoreFiles m = forM_ (M.toList m) $ \(rf, Mergeful.Timed SyncFile {..} st) -> do
  let p = contentsDir </> rf
  if filePred rf
    then do
      mContents <- liftIO $ forgivingAbsence $ SB.readFile $ fromAbsFile $ contentsDir </> rf
      -- Insert the metadata in any case
      runDB $
        insert_
          ClientFile
            { clientFilePath = rf,
              clientFileSha256 = SHA256.hashBytes syncFileContents,
              clientFileTime = st
            }
      case mContents of
        -- We don't have this file yet. Save it.
        Nothing -> do
          logDebugN $ "Saving file because we got it from the server and we didn't have it yet: " <> T.pack (fromAbsFile p)
          liftIO $ do
            ensureDir $ parent p
            SB.writeFile (fromAbsFile p) syncFileContents
        -- We already have a file at this path, don't overwrite it. It will be marked as 'changed' in the next sync.
        Just _ -> logDebugN $ "Not writing to file that we got from the server because we already have a file at that path: " <> T.pack (fromAbsFile p)
    else logDebugN $ "Not considering file because it was supposed to be hidden: " <> T.pack (fromAbsFile p)
  where
    filePred = case ignoreFiles of
      IgnoreNothing -> const True
      IgnoreHiddenFiles -> not . isHidden

clientMergeSyncResponse :: Path Abs Dir -> Path Abs Dir -> IgnoreFiles -> Mergeful.SyncResponse (Path Rel File) (Path Rel File) SyncFile -> C ()
clientMergeSyncResponse contentsDir backupDir ignoreFiles = runDB . Mergeful.mergeSyncResponseCustom Mergeful.mergeFromServerStrategy proc
  where
    proc :: Mergeful.ClientSyncProcessor (Path Rel File) (Path Rel File) SyncFile (SqlPersistT C)
    proc = Mergeful.ClientSyncProcessor {..}
      where
        clientSyncProcessorQuerySyncedButChangedValues :: Set (Path Rel File) -> SqlPersistT C (Map (Path Rel File) (Mergeful.Timed SyncFile))
        clientSyncProcessorQuerySyncedButChangedValues s = fmap (M.fromList . catMaybes) $ forM (S.toList s) $ \rf -> do
          mcf <- getBy (UniquePath rf)
          case mcf of
            Nothing -> pure Nothing
            Just (Entity _ ClientFile {..}) -> do
              mContents <- liftIO $ forgivingAbsence $ SB.readFile $ fromAbsFile $ contentsDir </> clientFilePath
              case mContents of
                Nothing -> pure Nothing
                Just contents -> do
                  let sfm = SyncFileMeta {syncFileMetaHash = clientFileSha256, syncFileMetaTime = clientFileTime}
                  if isUnchanged sfm contents
                    then pure Nothing
                    else do
                      let sf = SyncFile {syncFileContents = contents}
                      let tsf = Mergeful.Timed sf clientFileTime
                      pure $ Just (rf, tsf)
        clientSyncProcessorSyncClientAdded :: Map (Path Rel File) (Mergeful.ClientAddition (Path Rel File)) -> SqlPersistT C ()
        clientSyncProcessorSyncClientAdded m = forM_ (M.toList m) $ \(path, Mergeful.ClientAddition {..}) -> do
          let p = contentsDir </> path
          mContents <- liftIO $ forgivingAbsence $ SB.readFile $ fromAbsFile p
          case mContents of
            Nothing -> pure ()
            Just contents -> do
              logInfoN $ "Adding a client-added item locally, not on disk (because it's already there) but only its metadata: " <> T.pack (fromAbsFile p)
              insert_ ClientFile {clientFilePath = path, clientFileSha256 = SHA256.hashBytes contents, clientFileTime = clientAdditionServerTime}
        clientSyncProcessorSyncClientChanged :: Map (Path Rel File) Mergeful.ServerTime -> SqlPersistT C ()
        clientSyncProcessorSyncClientChanged m = forM_ (M.toList m) $ \(rf, st) -> do
          mcf <- getBy (UniquePath rf)
          case mcf of
            Nothing -> pure ()
            Just (Entity _ ClientFile {..}) -> do
              let p = contentsDir </> clientFilePath
              mContents <- liftIO $ forgivingAbsence $ SB.readFile $ fromAbsFile p
              case mContents of
                Nothing -> pure ()
                Just contents -> do
                  logInfoN $ "Updating a client-changed item locally, not on disk (because it's already been changed there) but only its metadata: " <> T.pack (fromAbsFile p)
                  updateWhere [ClientFilePath ==. rf] [ClientFileTime =. st, ClientFileSha256 =. SHA256.hashBytes contents]
        clientSyncProcessorSyncClientDeleted :: Set (Path Rel File) -> SqlPersistT C ()
        clientSyncProcessorSyncClientDeleted s = forM_ (S.toList s) $ \rf -> do
          mcf <- getBy (UniquePath rf)
          case mcf of
            Nothing -> pure ()
            Just (Entity cfid ClientFile {..}) -> do
              let p = contentsDir </> clientFilePath
              logInfoN $ "Deleting a client-deleted item locally, not on disk (because it's already gone there) but only its metadata: " <> T.pack (fromAbsFile p)
              delete cfid
        clientSyncProcessorSyncClientDeletedConflictTakeRemoteChanged :: Map (Path Rel File) (Mergeful.Timed SyncFile) -> SqlPersistT C ()
        clientSyncProcessorSyncClientDeletedConflictTakeRemoteChanged m = forM_ (M.toList m) $ \(rf, Mergeful.Timed SyncFile {..} st) -> when (filePred rf) $ do
          let p = contentsDir </> rf
          logInfoN $ "Adding a client-deleted-conflict item that needs to be added locally, both on disk and its metadata: " <> T.pack (fromAbsFile p)
          liftIO $ do
            ensureDir $ parent p
            SB.writeFile (fromAbsFile p) syncFileContents
          insert_ ClientFile {clientFilePath = rf, clientFileSha256 = SHA256.hashBytes syncFileContents, clientFileTime = st}
        clientSyncProcessorSyncClientDeletedConflictStayDeleted :: Map (Path Rel File) (Mergeful.Timed SyncFile) -> ReaderT SqlBackend C ()
        clientSyncProcessorSyncClientDeletedConflictStayDeleted _ = pure ()
        clientSyncProcessorSyncServerDeletedConflictKeepLocalChange :: Set (Path Rel File) -> ReaderT SqlBackend C ()
        clientSyncProcessorSyncServerDeletedConflictKeepLocalChange _ = pure ()
        clientSyncProcessorSyncServerDeletedConflictDelete :: Set (Path Rel File) -> SqlPersistT C ()
        clientSyncProcessorSyncServerDeletedConflictDelete s = forM_ (S.toList s) $ \rf -> do
          mcf <- getBy (UniquePath rf)
          case mcf of
            Nothing -> pure ()
            Just (Entity cfid ClientFile {..}) -> do
              let p = contentsDir </> clientFilePath
              logInfoN $ "Backing up a server-deleted-conflict item locally before deleting it: " <> T.pack (fromAbsFile p)
              lift $ backupFile contentsDir backupDir rf
              logInfoN $ "Deleting a server-deleted-conflict item that has to be deleted locally, both on disk and its metadata: " <> T.pack (fromAbsFile p)
              ignoringAbsence $ removeFile p
              delete cfid
        clientSyncProcessorSyncChangeConflictKeepLocal :: Map (Path Rel File) (Mergeful.Timed SyncFile) -> SqlPersistT C ()
        clientSyncProcessorSyncChangeConflictKeepLocal _ = pure ()
        clientSyncProcessorSyncChangeConflictMerged :: Map (Path Rel File) (Mergeful.Timed SyncFile) -> SqlPersistT C ()
        clientSyncProcessorSyncChangeConflictMerged m = forM_ (M.toList m) $ \(rf, Mergeful.Timed SyncFile {..} st) -> when (filePred rf) $ do
          let p = contentsDir </> rf
          logInfoN $ "Backing up a change-conflict item locally before merging it: " <> T.pack (fromAbsFile p)
          lift $ backupFile contentsDir backupDir rf
          logInfoN $ "Updating a merged change-conflict item locally on disk but not its metadata: " <> T.pack (fromAbsFile p)
          liftIO $ do
            ensureDir $ parent p
            SB.writeFile (fromAbsFile p) syncFileContents
          -- Don't update the hashes so the item stays marked as 'changed'
          updateWhere [ClientFilePath ==. rf] [ClientFileTime =. st]
        clientSyncProcessorSyncChangeConflictTakeRemote :: Map (Path Rel File) (Mergeful.Timed SyncFile) -> SqlPersistT C ()
        clientSyncProcessorSyncChangeConflictTakeRemote m = forM_ (M.toList m) $ \(rf, Mergeful.Timed SyncFile {..} st) -> when (filePred rf) $ do
          let p = contentsDir </> rf
          logInfoN $ "Backing up a change-conflict item locally before taking it from the remote: " <> T.pack (fromAbsFile p)
          lift $ backupFile contentsDir backupDir rf
          logInfoN $ "Updating a change-conflict item that will be taken from the remote locally, both on disk and its metadata: " <> T.pack (fromAbsFile p)
          liftIO $ do
            ensureDir $ parent p
            SB.writeFile (fromAbsFile p) syncFileContents
          updateWhere [ClientFilePath ==. rf] [ClientFileSha256 =. SHA256.hashBytes syncFileContents, ClientFileTime =. st]
        clientSyncProcessorSyncServerAdded :: Map (Path Rel File) (Mergeful.Timed SyncFile) -> SqlPersistT C ()
        clientSyncProcessorSyncServerAdded m = forM_ (M.toList m) $ \(rf, Mergeful.Timed SyncFile {..} st) -> when (filePred rf) $ do
          let p = contentsDir </> rf
          logInfoN $ "Adding a server-added item locally, both on disk and its metadata: " <> T.pack (fromAbsFile p)
          liftIO $ do
            ensureDir $ parent p
            SB.writeFile (fromAbsFile p) syncFileContents
          insert_ ClientFile {clientFilePath = rf, clientFileSha256 = SHA256.hashBytes syncFileContents, clientFileTime = st}
        clientSyncProcessorSyncServerChanged :: Map (Path Rel File) (Mergeful.Timed SyncFile) -> SqlPersistT C ()
        clientSyncProcessorSyncServerChanged m = forM_ (M.toList m) $ \(rf, Mergeful.Timed SyncFile {..} st) -> when (filePred rf) $ do
          let p = contentsDir </> rf
          logInfoN $ "Updating a server-changed item locally, both on disk and its metadata: " <> T.pack (fromAbsFile p)
          liftIO $ do
            ensureDir $ parent p
            SB.writeFile (fromAbsFile p) syncFileContents
          updateWhere [ClientFilePath ==. rf] [ClientFileSha256 =. SHA256.hashBytes syncFileContents, ClientFileTime =. st]
        clientSyncProcessorSyncServerDeleted :: Set (Path Rel File) -> SqlPersistT C ()
        clientSyncProcessorSyncServerDeleted s = forM_ (S.toList s) $ \rf -> do
          mcf <- getBy (UniquePath rf)
          case mcf of
            Nothing -> pure ()
            Just (Entity cfid ClientFile {..}) -> do
              let p = contentsDir </> clientFilePath
              logInfoN $ "Deleting a server-deleted item locally, both on disk and its metadata: " <> T.pack (fromAbsFile p)
              ignoringAbsence $ removeFile p
              delete cfid
        filePred = case ignoreFiles of
          IgnoreNothing -> const True
          IgnoreHiddenFiles -> not . isHidden

logInfoJsonData :: ToJSON a => Text -> a -> C ()
logInfoJsonData name a =
  logInfoN $ T.unwords [name <> ":", TE.decodeUtf8 $ LB.toStrict $ encodePretty a]

logDebugData :: Show a => Text -> a -> C ()
logDebugData name a = logDebugN $ T.unwords [name <> ":", T.pack $ ppShow a]

readServerUUID :: Path Abs File -> IO (Maybe ServerUUID)
readServerUUID p = do
  mContents <- forgivingAbsence $ LB.readFile $ toFilePath p
  forM mContents $ \contents ->
    case JSON.eitherDecode contents of
      Left err -> die err
      Right store -> pure store

writeServerUUID :: Path Abs File -> ServerUUID -> IO ()
writeServerUUID p u = do
  ensureDir (parent p)
  LB.writeFile (fromAbsFile p) $ JSON.encodePretty u

-- We will trust hashing. (TODO do we need to fix that?)
isUnchanged :: SyncFileMeta -> ByteString -> Bool
isUnchanged SyncFileMeta {..} contents =
  SHA256.hashBytes contents == syncFileMetaHash

backupFile :: Path Abs Dir -> Path Abs Dir -> Path Rel File -> C ()
backupFile contentsDir backupDir sourceFile = do
  now <- liftIO getCurrentTime
  let timeStr = formatTime defaultTimeLocale ".%F_%T" now
  destinationFile <- addFileExtension timeStr sourceFile
  let source = contentsDir </> sourceFile
  let destination = backupDir </> destinationFile
  me <- forgivingAbsence $ doesFileExist destination
  case me of
    Just True ->
      logErrorN $ "Failed to back up " <> T.pack (fromAbsFile source) <> " because the destination was already taken: " <> T.pack (fromAbsFile destination)
    _ -> do
      logInfoN $ "Backing up " <> T.pack (fromAbsFile source) <> " to " <> T.pack (fromAbsFile destination)
      ensureDir $ parent destination
      copyFile source destination
