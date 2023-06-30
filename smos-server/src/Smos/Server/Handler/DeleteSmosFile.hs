module Smos.Server.Handler.DeleteSmosFile
  ( serveDeleteSmosFile,
  )
where

import Path
import Smos.Data
import Smos.Server.Handler.Import

serveDeleteSmosFile :: AuthNCookie -> Path Rel File -> ServerHandler NoContent
serveDeleteSmosFile ac p = withUserId ac $ \uid -> do
  runDB $ deleteBy (UniqueServerFilePath uid p)
  pure NoContent
