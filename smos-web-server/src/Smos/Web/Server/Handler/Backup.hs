{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Web.Server.Handler.Backup where

import Data.Time
import Data.Word
import Smos.Client
import Smos.Web.Server.Foundation
import Smos.Web.Server.Widget
import Text.Printf
import Text.Time.Pretty
import Yesod hiding (Header)

getBackupsR :: Handler Html
getBackupsR = withLogin $ \t -> do
  now <- liftIO getCurrentTime
  backups <- runClientOrErr $ clientGetListBackups t
  withNavBar $ do
    token <- genToken
    $(widgetFile "backups")

postBackupR :: Handler Html
postBackupR = withLogin $ \t -> do
  _ <- runClientOrErr $ clientPostBackup t
  redirect BackupsR

postBackupRestoreR :: BackupUUID -> Handler Html
postBackupRestoreR uuid = withLogin $ \t -> do
  NoContent <- runClientOrErr $ clientPutRestoreBackup t uuid
  redirect BackupsR

prettySize :: Word64 -> String
prettySize w =
  let d = fromIntegral w :: Double -- Safe because it is Word64 -> Double and we don't care about precision
      kiloBytes = d / 1024
      megaBytes = kiloBytes / 1024
   in if megaBytes >= 1
        then printf "%.2f MiB" megaBytes
        else printf "%.2f KiB" kiloBytes
