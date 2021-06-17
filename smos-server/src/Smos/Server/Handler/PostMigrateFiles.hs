module Smos.Server.Handler.PostMigrateFiles
  ( servePostMigrateFiles,
  )
where

import Smos.Server.FileMigration
import Smos.Server.Handler.Import

servePostMigrateFiles :: AuthNCookie -> ServerHandler NoContent
servePostMigrateFiles ac = withUserId ac $ \uid -> do
  runDB $ runFileMigrationForUser uid
  pure NoContent
