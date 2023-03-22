module Smos.Cursor.Report.Streaming where

import Conduit
import Path
import Smos.Data
import Smos.Directory.Archive
import Smos.Directory.OptParse.Types
import Smos.Directory.Resolution
import Smos.Directory.ShouldPrint
import Smos.Directory.Streaming

produceReportCursorEntries :: MonadIO m => ConduitT (Path Rel File, SmosFile) a m () -> DirectorySettings -> m [a]
produceReportCursorEntries func dc = do
  wd <- liftIO $ resolveDirWorkflowDir dc
  runConduit $
    streamSmosFilesFromWorkflowRel HideArchive dc
      .| filterSmosFilesRel
      .| parseSmosFilesRel wd
      .| printShouldPrint DontPrint
      .| func
      .| sinkList
