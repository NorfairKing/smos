{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Notify where

import Conduit
import Control.Monad
import Control.Monad.Logger
import qualified Data.Conduit.Combinators as C
import Data.Hashable
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Database.Persist
import Database.Persist.Sqlite
import GHC.Generics (Generic)
import Path
import Path.IO
import Paths_smos_notify
import Smos.Data
import Smos.Notify.DB
import Smos.Notify.OptParse
import Smos.Report.Archive
import Smos.Report.Config
import Smos.Report.ShouldPrint
import Smos.Report.Streaming
import System.Exit
import System.IO
import System.Process
import Text.Show.Pretty (ppShow)
import Text.Time.Pretty

smosNotify :: IO ()
smosNotify = do
  Settings {..} <- getSettings
  runStderrLoggingT $
    filterLogger (\_ ll -> ll >= setLogLevel) $ do
      notifySendExecutable <- findNotifySend
      mPlayExecutable <- findPlay
      now <- liftIO getZonedTime
      wd <- liftIO $ resolveDirWorkflowDir setDirectorySettings
      logDebugN $ T.pack $ unwords ["Opening database at", fromAbsFile setDatabase]
      withSqlitePool (T.pack (fromAbsFile setDatabase)) 1 $ \pool ->
        flip runSqlPool pool $ do
          runMigration notifyMigration
          notificationEvents <-
            liftIO $ do
              runConduit $
                streamSmosFilesFromWorkflowRel HideArchive setDirectorySettings
                  .| parseSmosFilesRel wd
                  .| printShouldPrint (PrintWarning stderr)
                  .| smosFileEntries
                  .| C.concatMap (uncurry (parseNotificationEvent now))
                  .| sinkList
          notificationsToSend <- fmap catMaybes $
            forM notificationEvents $ \ne -> do
              let h = hash ne
              mn <- getBy (UniqueSentNotification h)
              case mn of
                Just _ -> do
                  logDebugN $ T.pack $ unwords ["Not sending notification for event with hash", show h, "because it has already had a notification sent."]
                  pure Nothing -- Already sent, not sending another notification
                Nothing -> pure $ Just ne
          -- Don't play a sound if there are no notifications to send.
          logDebugN $ T.pack $ unwords ["Sending", show (length notificationsToSend), "notifications."]
          unless (null notificationsToSend) $ do
            forM_ notificationsToSend $ \ne -> do
              logInfoN $ T.pack $ unlines ["Sending notification:", ppShow ne]
              displayNotification notifySendExecutable (renderNotification now ne)
              let h = hash ne
              logDebugN $ T.pack $ unwords ["Inserting notification with hash", show h]
              insert_
                SentNotification
                  { sentNotificationHash = h,
                    sentNotificationTime = zonedTimeToUTC now
                  }
            mapM_ playDing mPlayExecutable

data NotificationEvent
  = NotifyBegin
      Header
      (Maybe Contents)
      Timestamp
  deriving (Show, Eq, Generic)

instance Hashable NotificationEvent where
  hashWithSalt salt = \case
    NotifyBegin h mc ts ->
      hashWithSalt salt $
        T.concat
          [ headerText h,
            maybe T.empty contentsText mc,
            timestampText ts
          ]

renderNotification :: ZonedTime -> NotificationEvent -> Notification
renderNotification now = \case
  NotifyBegin h mc ts ->
    let nowUTC = zonedTimeToUTC now
        tsUTC = localTimeToUTC (zonedTimeZone now) $ timestampLocalTime ts
     in Notification
          { notificationSummary =
              T.unwords
                [ T.pack $ prettyTimeAuto nowUTC tsUTC <> ":",
                  headerText h
                ],
            notificationBody = contentsText <$> mc
          }

-- | What we send to notify-send
data Notification = Notification
  { notificationSummary :: Text,
    notificationBody :: Maybe Text
  }
  deriving (Show, Eq, Generic)

parseNotificationEvent :: ZonedTime -> Path Rel File -> Entry -> Maybe NotificationEvent
parseNotificationEvent now _ e = do
  guard (not (isDone (entryState e)))
  beginTS <- M.lookup "BEGIN" (entryTimestamps e)
  let nowUTC = zonedTimeToUTC now
  let beginUTC = localTimeToUTC (zonedTimeZone now) $ timestampLocalTime beginTS
  let d = diffUTCTime beginUTC nowUTC
  let minutesAhead = 5
      timestampIsSoon = d >= 0 && d <= minutesAhead * 60
  guard timestampIsSoon
  pure $ NotifyBegin (entryHeader e) (entryContents e) beginTS

isDone :: Maybe TodoState -> Bool
isDone (Just "DONE") = True
isDone (Just "CANCELLED") = True
isDone (Just "FAILED") = True
isDone _ = False

findNotifySend :: (MonadLogger m, MonadIO m) => m (Path Abs File)
findNotifySend = do
  rp <- liftIO $ parseRelFile "notify-send"
  me <- findExecutable rp
  case me of
    Nothing -> liftIO $ die "could not find a notify-send executable."
    Just e -> do
      logDebugN $ T.pack $ unwords ["Found notify-send executable at:", fromAbsFile e]
      pure e

findPlay :: (MonadLogger m, MonadIO m) => m (Maybe (Path Abs File))
findPlay = do
  rp <- liftIO $ parseRelFile "play"
  me <- findExecutable rp
  forM_ me $ \e ->
    logDebugN $ T.pack $ unwords ["Found play executable at:", fromAbsFile e]
  pure me

displayNotification :: (MonadLogger m, MonadIO m) => Path Abs File -> Notification -> m ()
displayNotification e Notification {..} = do
  dd <- liftIO getDataDir
  let logoFile = dd ++ "/assets/logo.png"
  logDebugN $ T.pack $ unwords ["Displaying", fromAbsFile e, T.unpack notificationSummary]
  liftIO $
    callProcess
      (fromAbsFile e)
      $ [ "--urgency=critical",
          "--app-name=smos",
          "--icon=" ++ logoFile,
          T.unpack notificationSummary
        ]
        ++ maybeToList (T.unpack <$> notificationBody)

playDing :: (MonadLogger m, MonadIO m) => Path Abs File -> m ()
playDing e = do
  dd <- liftIO getDataDir
  let soundFile = dd ++ "/assets/ting.wav"
  logDebugN $ T.pack $ unwords ["Playing", fromAbsFile e, soundFile]
  liftIO $
    callProcess
      (fromAbsFile e)
      [ "--no-show-progress",
        "--volume",
        "0.5",
        soundFile
      ]
