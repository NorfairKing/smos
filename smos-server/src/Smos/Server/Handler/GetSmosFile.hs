{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.GetSmosFile
  ( serveGetSmosFile,
  )
where

import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Path
import Smos.Data
import Smos.Server.Handler.Import

serveGetSmosFile :: AuthNCookie -> Path Rel File -> ServerHandler SmosFile
serveGetSmosFile AuthNCookie {..} p = withUserId authCookieUsername $ \uid -> do
  msf <- runDB $ getBy (UniqueServerFilePath uid p)
  case msf of
    Nothing -> throwError err404
    Just (Entity _ ServerFile {..}) ->
      case parseSmosFile serverFileContents of
        Left err ->
          throwError $
            err400
              { errBody =
                  "A file exits at this path but it is not a valid smos file: "
                    <> LB.fromStrict (TE.encodeUtf8 (T.pack err))
              }
        Right sf -> pure sf
