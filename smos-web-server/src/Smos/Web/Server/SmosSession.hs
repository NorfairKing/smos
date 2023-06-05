{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Smos.Web.Server.SmosSession where

import Conduit
import Control.Monad.Logger
import qualified Data.Text as T
import Database.Persist.Sqlite as DB
import Path
import Path.IO
import Smos.CLI.Logging
import Smos.Client hiding (Header)
import Smos.Default
import Smos.Directory.OptParse.Types
import Smos.Instance
import Smos.Report.OptParse.Types
import Smos.Sync.Client.Command.Sync
import Smos.Sync.Client.OptParse.Types
import Smos.Terminal
import Smos.Types
import Smos.Web.Server.Foundation
import UnliftIO hiding (Handler)
import Yesod hiding (Header)

withPlaygroundSession ::
  (MonadUnliftIO m) =>
  Path Rel File ->
  (TerminalHandle -> m a) ->
  m a
withPlaygroundSession relFile func =
  withPlaygroundDir $ \workflowDir ->
    withSmosSessionIn workflowDir (StartingFile $ workflowDir </> relFile) func

{-# ANN withPlaygroundDir ("NOCOVER" :: String) #-}
withPlaygroundDir :: forall m a. (MonadUnliftIO m) => (Path Abs Dir -> m a) -> m a
withPlaygroundDir func = withRunInIO $ \runInIO ->
  withSystemTempDir "smos-web-server-playground" $ \tempDir ->
    runInIO $ bracket_ (ensureDir tempDir) (removeDirRecur tempDir) (func (toWorkflowDir tempDir))
  where
    toWorkflowDir :: Path Abs Dir -> Path Abs Dir
    toWorkflowDir = (</> [reldir|workflow|])

withSmosSession ::
  (MonadUnliftIO m, MonadHandler m, HandlerSite m ~ App) =>
  Username ->
  Token ->
  (TerminalHandle -> m a) ->
  m a
withSmosSession userName token func =
  withReadiedDir userName token $ \workflowDir ->
    withSmosSessionIn workflowDir (StartingDir workflowDir) func

withSmosSessionIn ::
  MonadUnliftIO m =>
  Path Abs Dir ->
  StartingPath ->
  (TerminalHandle -> m a) ->
  m a
withSmosSessionIn workflowDir sp func = do
  let reportConfig = reportConfigFor workflowDir
  let config =
        defaultConfig
          { configReportSettings = reportConfig,
            configExplainerMode = True,
            configSandboxMode = True
          }
  withSmosInstance config (Just sp) func

reportConfigFor ::
  Path Abs Dir -> ReportSettings
reportConfigFor workflowDir =
  defaultReportSettings
    { reportSettingDirectorySettings = directoryConfigFor workflowDir
    }

directoryConfigFor ::
  Path Abs Dir -> DirectorySettings
directoryConfigFor workflowDir =
  defaultDirectorySettings
    { directoryConfigWorkflowFileSpec = AbsoluteWorkflow workflowDir
    }

withReadiedDir :: forall m a. (MonadUnliftIO m, MonadHandler m, HandlerSite m ~ App) => Username -> Token -> (Path Abs Dir -> m a) -> m a
withReadiedDir userName token func = bracket readyDir unreadyDir (func . toWorkflowDir)
  where
    toWorkflowDir :: Path Abs Dir -> Path Abs Dir
    toWorkflowDir = (</> [reldir|workflow|])
    readyDir :: m (Path Abs Dir)
    readyDir = do
      userDir <- userDataDir userName
      let workflowDir = toWorkflowDir userDir
      ensureDir workflowDir
      doSync userDir
      pure userDir
    unreadyDir :: Path Abs Dir -> m ()
    unreadyDir userDir = do
      doSync userDir
    doSync :: Path Abs Dir -> m ()
    doSync userDir = do
      let workflowDir = toWorkflowDir userDir
      backupDir <- resolveDir userDir "sync-conflict-backups"
      dbFile <- resolveFile userDir "sync-metadata.sqlite3"
      uuidFile <- resolveFile userDir "server-uuid.json"
      man <- getsYesod appHttpManager
      burl <- getsYesod appAPIBaseUrl
      let cenv = mkClientEnv man burl
      liftIO $
        runFilteredLogger LevelWarn $
          DB.withSqlitePool (T.pack $ fromAbsFile dbFile) 1 $ \pool ->
            doActualSync uuidFile pool workflowDir IgnoreHiddenFiles backupDir cenv token
