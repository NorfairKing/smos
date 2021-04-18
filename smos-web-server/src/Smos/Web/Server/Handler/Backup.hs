{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Web.Server.Handler.Backup
  ( getBackupsR,
    postBackupR,
    getBackupDownloadR,
    postBackupRestoreR,
  )
where

import Conduit
import Control.Monad.Except
import qualified Data.ByteString.Lazy as LB
import Data.Time
import Data.Word
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types as Http
import Servant.Types.SourceT as Source
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
  errOrBackup <- runClientSafe $ clientPostBackup t
  case errOrBackup of
    Left err -> handleStandardServantErrs err $ \resp ->
      if Http.statusCode (responseStatusCode resp) == 403
        then do
          addMessage "Maximum number of backups reached." "Maximum number of backups reached."
          getBackupsR
        else redirect BackupsR
    Right _ -> redirect BackupsR

getBackupDownloadR :: BackupUUID -> Handler TypedContent
getBackupDownloadR uuid = withLogin $ \t -> do
  archiveBytestring <- runClientOrErr $ do
    clientSource <- clientGetBackup t uuid
    -- TODO; this reads the entire archive into memory, which is probably not what we want to be doing.
    -- Try a streaming approach, but remember; we tried in 'b2badc3e80587f588d1eb6819ba673e07e7964f7' and it was a big mess.
    errOrLB <- liftIO $ runExceptT $ LB.fromChunks <$> runSourceT clientSource
    case errOrLB of
      Left err -> liftIO $ fail err
      Right lb -> pure lb
  Yesod.addHeader "Content-Disposition" "attachment; filename=\"smos-backup.zip\""
  Yesod.respond "application/zip" archiveBytestring

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
