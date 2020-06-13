{-# LANGUAGE OverloadedStrings #-}

module Smos.Server.Handler.PutSmosFile
  ( servePutSmosFile,
  )
where

import qualified Data.Mergeful as Mergeful
import Path
import Smos.Data
import Smos.Server.Handler.Import

servePutSmosFile :: AuthCookie -> Path Rel File -> SmosFile -> SyncHandler NoContent
servePutSmosFile (AuthCookie un) p sf = do
  mu <- runDB $ getBy $ UniqueUsername un
  case mu of
    Nothing -> throwError err404
    Just (Entity uid _) -> do
      uuid <- nextRandomUUID
      let contentsBS = smosFileYamlBS sf
          record =
            ServerFile
              { serverFileUser = uid,
                serverFileUuid = uuid,
                serverFilePath = p,
                serverFileContents = contentsBS,
                serverFileTime = Mergeful.initialServerTime
              }
          updates = [ServerFileContents =. contentsBS, ServerFileTime +=. Mergeful.ServerTime 1]
      void $ runDB $ upsertBy (UniqueServerFilePath uid p) record updates
      pure NoContent
