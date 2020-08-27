{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Smos.Web.Server.SmosSession where

import Conduit
import Control.Monad
import Control.Monad.Logger
import Data.Aeson as Aeson
import Data.Aeson.Text as JSON
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Persist.Sqlite as DB
import Network.HTTP.Types.Status (badRequest400)
import Path
import Path.IO
import Smos.Client hiding (Header)
import Smos.Data
import Smos.Sync.Client.Command.Sync
import Smos.Sync.Client.OptParse.Types
import Smos.Web.Server.Foundation
import Smos.Web.Server.SmosInstance
import Smos.Web.Server.Static
import Smos.Web.Server.Widget
import Text.Julius
import Text.Show.Pretty (ppShow)
import UnliftIO hiding (Handler)
import Yesod hiding (Header)
import Yesod.WebSockets

withSmosSession :: (MonadUnliftIO m, MonadHandler m, HandlerSite m ~ App) => Username -> Token -> TVar (Map Username SmosInstanceHandle) -> (SmosInstanceHandle -> m a) -> m a
withSmosSession userName token instancesVar func = do
  mInstance <- M.lookup userName <$> readTVarIO instancesVar
  case mInstance of
    Nothing -> withReadiedDir userName token $ \workflowDir -> do
      startingFile <- liftIO $ resolveFile workflowDir "example.smos"
      withSmosInstance workflowDir startingFile func
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
      dataDir <- getsYesod appDataDir
      userDir <- resolveDir dataDir $ usernameToPath userName
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

usernameToPath :: Username -> FilePath
usernameToPath = T.unpack . toHexText . hashBytes . TE.encodeUtf8 . usernameText
