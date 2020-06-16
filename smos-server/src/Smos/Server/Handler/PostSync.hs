{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.PostSync
  ( servePostSync,
  )
where

import qualified Data.Map as M
import qualified Data.Mergeful as Mergeful
import qualified Data.Mergeful.Persistent as Mergeful
import Path
import Smos.Server.Handler.Import

servePostSync :: AuthCookie -> SyncRequest -> ServerHandler SyncResponse
servePostSync (AuthCookie un) SyncRequest {..} = withUserId un $ \uid -> do
  syncResponseServerId <- asks serverEnvServerUUID
  modifiedAddedItems <- runDB $ fmap (M.mapMaybe id) $ flip M.traverseWithKey (Mergeful.syncRequestNewItems syncRequestItems) $ \p sf -> do
    mp <- getBy (UniqueServerFilePath uid p)
    case mp of
      -- Doesn't exist yet, just leave it.
      Nothing -> do
        liftIO $ putStrLn $ "Leaving " <> show p
        pure $ Just sf
      -- Exists already, remove it from the request.
      Just _ -> do
        liftIO $ putStrLn $ "Removing " <> show p
        pure Nothing
  let modifiedSyncRequest = syncRequestItems {Mergeful.syncRequestNewItems = modifiedAddedItems}
  syncResponseItems <- undefined
  -- runDB $
  --   Mergeful.serverProcessSyncWithCustomIdQuery
  --     ServerFileUuid
  --     nextRandomUUID
  --     ServerFileTime
  --     [ServerFileUser ==. uid]
  --     readSyncFile
  --     (writeSyncFile uid)
  --     syncFileUpdates
  --     modifiedSyncRequest
  pure SyncResponse {..}
  where
    readSyncFile :: ServerFile -> (Path Rel File, Mergeful.Timed SyncFile)
    readSyncFile ServerFile {..} =
      let sf = SyncFile {syncFileContents = serverFileContents}
       in (serverFilePath, Mergeful.Timed sf serverFileTime)
    writeSyncFile :: UserId -> Path Rel File -> SyncFile -> ServerFile
    writeSyncFile uid path SyncFile {..} =
      ServerFile
        { serverFileUser = uid,
          serverFilePath = path,
          serverFileContents = syncFileContents,
          serverFileTime = Mergeful.initialServerTime
        }
    syncFileUpdates :: SyncFile -> [Update ServerFile]
    syncFileUpdates SyncFile {..} = [ServerFileContents =. syncFileContents]
