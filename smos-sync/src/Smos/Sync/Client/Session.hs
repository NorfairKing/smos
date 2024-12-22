module Smos.Sync.Client.Session
  ( loadToken,
    loadSession,
    saveSession,
  )
where

import Control.Monad.IO.Class
import qualified Data.ByteString as SB
import qualified Data.ByteString.Builder as SBB
import qualified Data.ByteString.Lazy as LB
import Path
import Path.IO
import Servant.Auth.Client
import UnliftIO.IO.File
import Web.Cookie

loadToken :: (MonadIO m) => Path Abs File -> m (Maybe Token)
loadToken tp = do
  mCookie <- loadSession tp
  pure $ Token . setCookieValue <$> mCookie

loadSession :: (MonadIO m) => Path Abs File -> m (Maybe SetCookie)
loadSession p = do
  mContents <- liftIO $ forgivingAbsence $ SB.readFile $ toFilePath p
  pure $ parseSetCookie <$> mContents

saveSession :: (MonadIO m) => Path Abs File -> SetCookie -> m ()
saveSession p setCookie =
  liftIO $ do
    ensureDir $ parent p
    writeBinaryFileDurableAtomic (toFilePath p) $ LB.toStrict $ SBB.toLazyByteString $ renderSetCookie setCookie
