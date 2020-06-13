{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.PostSync
  ( servePostSync,
  )
where

import qualified Data.Mergeful as Mergeful
import qualified Data.Mergeful.Persistent as Mergeful
import Smos.Server.Handler.Import

servePostSync :: AuthCookie -> SyncRequest -> ServerHandler SyncResponse
servePostSync (AuthCookie un) request = withUserId un $ \uid -> do
  syncResponseServerId <- asks serverEnvServerUUID
  syncResponseItems <- runDB $ Mergeful.serverProcessSyncWithCustomIdQuery ServerFileUuid nextRandomUUID ServerFileTime [ServerFileUser ==. uid] readSyncFile (writeSyncFile uid) syncFileUpdates request
  pure SyncResponse {..}
  where
    readSyncFile :: ServerFile -> (FileUUID, Mergeful.Timed SyncFile)
    readSyncFile ServerFile {..} =
      let sf = SyncFile {syncFilePath = serverFilePath, syncFileContents = serverFileContents}
       in (serverFileUuid, Mergeful.Timed sf serverFileTime)
    writeSyncFile :: UserId -> FileUUID -> SyncFile -> ServerFile
    writeSyncFile uid uuid SyncFile {..} = ServerFile {serverFileUser = uid, serverFileUuid = uuid, serverFilePath = syncFilePath, serverFileContents = syncFileContents, serverFileTime = Mergeful.initialServerTime}
    syncFileUpdates :: SyncFile -> [Update ServerFile]
    syncFileUpdates SyncFile {..} = [ServerFilePath =. syncFilePath, ServerFileContents =. syncFileContents]
