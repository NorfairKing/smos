{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.PutSmosFile
  ( servePutSmosFile,
  )
where

import qualified Data.Mergeful as Mergeful
import Path
import Smos.Data
import Smos.Server.Handler.Import

servePutSmosFile :: AuthNCookie -> Path Rel File -> SmosFile -> ServerHandler NoContent
servePutSmosFile AuthNCookie {..} p sf = withUserId authCookieUsername $ \uid -> do
  let contentsBS = smosFileYamlBS sf
      record =
        ServerFile
          { serverFileUser = uid,
            serverFilePath = p,
            serverFileContents = contentsBS,
            serverFileTime = Mergeful.initialServerTime
          }
      updates = [ServerFileContents =. contentsBS, ServerFileTime +=. Mergeful.ServerTime 1]
  void $ runDB $ upsertBy (UniqueServerFilePath uid p) record updates
  pure NoContent
