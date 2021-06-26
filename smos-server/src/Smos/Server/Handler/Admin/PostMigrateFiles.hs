module Smos.Server.Handler.Admin.PostMigrateFiles
  ( servePostMigrateFiles,
  )
where

import Smos.Server.FileMigration
import Smos.Server.Handler.Import

servePostMigrateFiles :: AuthNCookie -> ServerHandler NoContent
servePostMigrateFiles ac = asAdmin (authNCookieUsername ac) $ do
  userIdsList <- runDB $ selectKeysList [] [Asc UserId]
  mapM_ (runDB . runFileMigrationForUser) userIdsList
  pure NoContent
