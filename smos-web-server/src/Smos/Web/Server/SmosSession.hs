module Smos.Web.Server.SmosSession where

import Conduit
import Control.Monad
import Data.Aeson as Aeson
import Data.Aeson.Text as JSON
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types.Status (badRequest400)
import Path
import Path.IO
import Smos.Client hiding (Header)
import Smos.Data
import Smos.Web.Server.Foundation
import Smos.Web.Server.SmosInstance
import Smos.Web.Server.Static
import Smos.Web.Server.Widget
import Text.Julius
import Text.Show.Pretty (ppShow)
import UnliftIO hiding (Handler)
import Yesod hiding (Header)
import Yesod.WebSockets

withSmosSession :: MonadUnliftIO m => Username -> TVar (Map Username SmosInstanceHandle) -> (SmosInstanceHandle -> m a) -> m a
withSmosSession un instancesVar func = do
  mInstance <- M.lookup un <$> readTVarIO instancesVar
  case mInstance of
    Nothing -> withReadiedDir un $ \workflowDir -> do
      startingFile <- liftIO $ resolveFile workflowDir "example.smos"
      withSmosInstance workflowDir startingFile func
    Just i -> do
      liftIO $ putStrLn "Should not happen."
      -- Should not happen, but it's fine if it does, I guess...
      -- We'll assume that whatever opened this instance will deal with cleanup as well.
      func i

withReadiedDir :: MonadUnliftIO m => Username -> (Path Abs Dir -> m a) -> m a
withReadiedDir un = bracket readyDir unreadyDir
  where
    readyDir :: MonadUnliftIO m => m (Path Abs Dir)
    readyDir = do
      liftIO $ putStrLn "Loading dir"
      liftIO $ putStrLn "Creating a new instance"
      dataDir <- resolveDir' "/tmp/smos-web-server/data"
      workflowDir <- resolveDir dataDir $ usernameToPath un
      -- TODO Load the dir
      ensureDir workflowDir
      pure workflowDir
    unreadyDir :: MonadUnliftIO m => Path Abs Dir -> m ()
    unreadyDir workflowDir = do
      liftIO $ putStrLn "Unloading dir"
      removeDirRecur workflowDir -- TODO save the dir

usernameToPath :: Username -> FilePath
usernameToPath = T.unpack . toHexText . hashBytes . TE.encodeUtf8 . usernameText
