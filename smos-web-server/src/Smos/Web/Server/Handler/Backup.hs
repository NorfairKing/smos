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
import Data.ByteString (ByteString)
import Data.ByteString.Builder as ByteString (Builder)
import Data.Time
import Data.Word
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
  _ <- runClientOrErr $ clientPostBackup t
  redirect BackupsR

getBackupDownloadR :: BackupUUID -> Handler TypedContent
getBackupDownloadR uuid = withLogin $ \t -> do
  withClient (clientGetBackup t uuid) $ \clientSource ->
    respondWithClientSource clientSource $ \responseSender -> do
      Yesod.addHeader "Content-Disposition" "attachment; filename=\"smos-backup.zip\""
      Yesod.respondSource "application/zip" responseSender

respondWithClientSource :: forall a. SourceIO ByteString -> (ConduitT () (Flush ByteString.Builder) Handler () -> Handler a) -> Handler a
respondWithClientSource (SourceT sourceFunc) func = withRunInIO $ \runHandlerInIO -> do
  liftIO $
    sourceFunc $ \step -> do
      liftIO $ print "starting to respond"
      let writerConduit :: ConduitT () (Flush ByteString.Builder) Handler ()
          writerConduit = do
            let go :: StepT IO ByteString -> ConduitT () (Flush ByteString.Builder) Handler ()
                go = \case
                  Stop -> do
                    liftIO $ print "step"
                    sendFlush
                  Error err -> liftIO $ fail err
                  Skip s -> do
                    liftIO $ print "skip"
                    go s
                  Yield x s -> do
                    liftIO $ print ("yield", x)
                    sendChunkBS x
                    go s
                  Effect ms -> do
                    liftIO $ print "effect"
                    s <- liftIO ms
                    go s
            go step
      r <- runHandlerInIO $ func writerConduit
      liftIO $ print "done responding"
      pure r

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
