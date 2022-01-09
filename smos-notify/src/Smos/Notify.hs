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
      ensureDir $ parent setDatabase
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
              let nowUTC = zonedTimeToUTC now
              -- Upsert insteam of insert just in case a two events have the
              -- same hash if we used insert here instead, the program would
              -- crash and the rest of the notifications wouldn't be sent.
              upsertBy
                (UniqueSentNotification h)
                SentNotification
                  { sentNotificationHash = h,
                    sentNotificationTime = nowUTC
                  }
                [SentNotificationTime =. nowUTC]
            mapM_ playDing mPlayExecutable

data NotificationEvent
  = NotifyTimestamp
      !(Path Rel File)
      !Header
      !(Maybe Contents)
      !TimestampName
      !Timestamp
  deriving (Show, Eq, Generic)

instance Hashable NotificationEvent where
  hashWithSalt salt = \case
    NotifyTimestamp rf h mc tsn ts ->
      hashWithSalt salt $
        T.concat
          [ T.pack (fromRelFile rf),
            headerText h,
            maybe T.empty contentsText mc,
            timestampNameText tsn,
            timestampText ts
          ]

renderNotification :: ZonedTime -> NotificationEvent -> Notification
renderNotification now = \case
  NotifyTimestamp _ h mc tsn ts ->
    let nowUTC = zonedTimeToUTC now
        tsLocalTime = timestampLocalTime ts
        tsUTC = localTimeToUTC (zonedTimeZone now) tsLocalTime
     in Notification
          { notificationSummary =
              T.unlines
                [ T.unwords
                    [ timestampNameText tsn,
                      T.pack (prettyTimeAuto nowUTC tsUTC) <> ", ",
                      "at",
                      T.pack (formatTime defaultTimeLocale "%H:%M" tsLocalTime <> ":")
                    ],
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

parseNotificationEvent :: ZonedTime -> Path Rel File -> Entry -> [NotificationEvent]
parseNotificationEvent now rf e = do
  guard (not (entryIsDone e))
  (tsn, ts) <- M.toList (entryTimestamps e)
  guard $ tsn `elem` ["SCHEDULED", "BEGIN", "DEADLINE"]
  let nowUTC = zonedTimeToUTC now
  lt <- case ts of
    TimestampDay _ -> [] -- Don't notify about day-based timestamps
    TimestampLocalTime lt -> [lt]
  let tsUTC = localTimeToUTC (zonedTimeZone now) lt
  let d = diffUTCTime tsUTC nowUTC
  let minutesAhead = 5
      timestampIsSoon = d >= 0 && d <= minutesAhead * 60
  guard timestampIsSoon
  guard $ case entryContents e of
    Nothing -> True
    Just cts -> not $ "SMOS_NO_NOTIFY" `T.isInfixOf` contentsText cts
  pure $ NotifyTimestamp rf (entryHeader e) (entryContents e) tsn ts

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
  let args =
        [ "--urgency=critical",
          "--app-name=smos",
          "--icon=" ++ logoFile,
          -- We must use '--' here so that any summary or body beginning with
          -- '-' chars is not interpreted as a flag.
          "--",
          T.unpack notificationSummary
        ]
          ++ maybeToList (T.unpack <$> notificationBody)
  logDebugN $ T.pack $ unwords $ fromAbsFile e : map show args
  liftIO $ callProcess (fromAbsFile e) args

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
