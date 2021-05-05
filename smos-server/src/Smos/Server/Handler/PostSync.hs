{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Smos.Server.Handler.PostSync
  ( servePostSync,
  )
where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Mergeful as Mergeful
import qualified Data.Text as T
import Path
import Smos.Server.Handler.Import

servePostSync :: AuthCookie -> SyncRequest -> ServerHandler SyncResponse
servePostSync AuthCookie {..} SyncRequest {..} = withUserId authCookieUsername $ \uid -> do
  syncResponseServerId <- asks serverEnvServerUUID
  syncResponseItems <- runDB $ Mergeful.processServerSyncCustom (syncProcessor uid) syncRequestItems
  logInfoN $ T.unwords ["Sync for user", T.pack (show (usernameText authCookieUsername))]
  pure SyncResponse {..}

syncProcessor :: forall m m'. (MonadIO m, m' ~ SqlPersistT m) => UserId -> Mergeful.ServerSyncProcessor (Path Rel File) (Path Rel File) SyncFile m'
syncProcessor uid = Mergeful.ServerSyncProcessor {..}
  where
    serverSyncProcessorRead :: m' (Map (Path Rel File) (Mergeful.Timed SyncFile))
    serverSyncProcessorRead = do
      sfs <- selectList [ServerFileUser ==. uid] []
      pure $
        M.fromList $
          flip map sfs $ \(Entity _ ServerFile {..}) ->
            ( serverFilePath,
              Mergeful.Timed
                { Mergeful.timedValue = SyncFile {syncFileContents = serverFileContents},
                  Mergeful.timedTime = serverFileTime
                }
            )
    serverSyncProcessorAddItem :: Path Rel File -> SyncFile -> m' (Maybe (Path Rel File))
    serverSyncProcessorAddItem path SyncFile {..} = do
      mk <-
        insertUnique
          ServerFile
            { serverFileUser = uid,
              serverFilePath = path,
              serverFileContents = syncFileContents,
              serverFileTime = Mergeful.initialServerTime
            }
      pure (path <$ mk)
    serverSyncProcessorChangeItem :: Path Rel File -> Mergeful.ServerTime -> SyncFile -> m' ()
    serverSyncProcessorChangeItem path st SyncFile {..} =
      void $
        upsertBy
          (UniqueServerFilePath uid path)
          ServerFile
            { serverFileUser = uid,
              serverFilePath = path,
              serverFileContents = syncFileContents,
              serverFileTime = st
            }
          [ServerFileContents =. syncFileContents, ServerFileTime =. st]
    serverSyncProcessorDeleteItem :: Path Rel File -> m' ()
    serverSyncProcessorDeleteItem path = deleteBy (UniqueServerFilePath uid path)
