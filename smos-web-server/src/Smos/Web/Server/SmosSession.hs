{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Smos.Web.Server.SmosSession where

import Conduit
import Control.Monad.Logger
import qualified Data.Map as M
import qualified Data.Text as T
import Database.Persist.Sqlite as DB
import Path
import Path.IO
import Smos.Client hiding (Header)
import Smos.Config
import Smos.Default
import Smos.Instance
import Smos.Sync.Client.Command.Sync
import Smos.Sync.Client.OptParse.Types
import Smos.Web.Server.Foundation
import UnliftIO hiding (Handler)
import Yesod hiding (Header)

withSmosSession ::
  (MonadUnliftIO m, MonadHandler m, HandlerSite m ~ App) =>
  Username ->
  Token ->
  TVar (Map Username SmosInstanceHandle) ->
  Path Rel File ->
  (SmosInstanceHandle -> m a) ->
  m a
withSmosSession userName token instancesVar relFile func = do
  mInstance <- M.lookup userName <$> readTVarIO instancesVar
  case mInstance of
    Nothing -> withReadiedDir userName token $ \workflowDir -> do
      let startingFile = workflowDir </> relFile
      let config =
            defaultConfig
              { configReportConfig =
                  defaultReportConfig
                    { smosReportConfigDirectoryConfig =
                        defaultDirectoryConfig
                          { directoryConfigWorkflowFileSpec = DirAbsolute workflowDir
                          }
                    }
              }
      withSmosInstance config startingFile func
    Just i -> do
      -- Should not happen, but it's fine if it does, I guess...
      -- We'll assume that whatever opened this instance will deal with cleanup as well.
      func i

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
      liftIO $ runStderrLoggingT
        $ filterLogger (\_ ll -> ll >= LevelWarn)
        $ DB.withSqlitePool (T.pack $ fromAbsFile dbFile) 1
        $ \pool ->
          doActualSync uuidFile pool workflowDir IgnoreHiddenFiles backupDir cenv token
