{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Notify where

import Conduit
import Data.Text (Text)
import qualified Data.Text as T
import Path
import Path.IO
import Smos.Notify.OptParse
import Smos.Report.Archive
import Smos.Report.Config
import Smos.Report.ShouldPrint
import Smos.Report.Streaming
import System.Exit
import System.IO
import System.Process

smosNotify :: IO ()
smosNotify = do
  -- sets@Settings {..} <- getSettings
  -- print sets
  -- wd <- resolveDirWorkflowDir setDirectorySettings
  -- fs <- runConduit $ streamSmosFilesFromWorkflowRel HideArchive setDirectorySettings .| parseSmosFilesRel wd .| printShouldPrint (PrintWarning stderr) .| smosFileEntries .| sinkList
  -- mapM_ print fs
  executable <- findNotifySend
  let example = Notification {notificationSummary = "hello world", notificationBody = "foo bar\nquux"}
  displayNotification executable example

data Notification = Notification
  { notificationSummary :: Text,
    notificationBody :: Text
  }
  deriving (Show, Eq)

findNotifySend :: IO (Path Abs File)
findNotifySend = do
  rp <- parseRelFile "notify-send"
  me <- findExecutable rp
  case me of
    Nothing -> die "could not find a notify-send executable."
    Just e -> pure e

displayNotification :: Path Abs File -> Notification -> IO ()
displayNotification e Notification {..} = do
  callProcess
    (fromAbsFile e)
    [ "--urgency=critical",
      "--app-name=smos",
      "--icon=/home/syd/src/cs-syd.eu/site/logo/positive/logo.png",
      T.unpack notificationSummary,
      T.unpack notificationBody
    ]
