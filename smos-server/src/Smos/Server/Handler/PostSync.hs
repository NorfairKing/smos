{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.PostSync
  ( servePostSync,
  )
where

import qualified Data.Mergeful as Mergeful
import Smos.Server.Handler.Import

servePostSync :: AuthCookie -> SyncRequest -> SyncHandler SyncResponse
servePostSync (AuthCookie un) request = do
  mu <- runDB $ getBy $ UniqueUsername un
  case mu of
    Nothing -> throwError err404
    Just (Entity uid _) -> do
      ServerEnv {..} <- ask
      store <- runDB $ readServerStore uid
      (respItems, newStore) <- Mergeful.processServerSync nextRandomUUID store request
      runDB $ writeServerStore uid newStore
      let resp =
            SyncResponse {syncResponseServerId = serverEnvServerUUID, syncResponseItems = respItems}
      pure resp
