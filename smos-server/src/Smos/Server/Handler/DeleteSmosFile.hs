module Smos.Server.Handler.DeleteSmosFile
  ( serveDeleteSmosFile,
  )
where

import Path
import Smos.Server.Handler.Import

serveDeleteSmosFile :: AuthNCookie -> Path Rel File -> ServerHandler NoContent
serveDeleteSmosFile ac p = withUserId ac $ \uid -> do
  mServerFile <- runDB $ getBy $ UniqueServerFilePath uid p
  case mServerFile of
    Nothing -> throwError err404
    Just (Entity sfid _) -> do
      runDB $ delete sfid
      pure NoContent
