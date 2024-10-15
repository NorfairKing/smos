{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Notify.OptParse (Settings (..), getSettings) where

import Control.Monad.Logger
import OptEnvConf
import Path
import Path.IO
import Paths_smos_notify (version)
import Smos.CLI.Logging ()
import Smos.CLI.OptParse
import Smos.Directory.OptParse
import System.Exit

getSettings :: IO Settings
getSettings = runSettingsParser version "Smos' notification tool"

data Settings = Settings
  { setDirectorySettings :: !DirectorySettings,
    setDatabase :: !(Path Abs File),
    setNotifySend :: !(Path Abs File),
    setLogLevel :: !LogLevel
  }

instance HasParser Settings where
  settingsParser = parseSettings

{-# ANN parseSettings ("NOCOVER" :: String) #-}
parseSettings :: OptEnvConf.Parser Settings
parseSettings = withSmosConfig $ do
  setDirectorySettings <- settingsParser
  let sub = subConfig_ "notify" . subEnv_ "notify"
  setDatabase <-
    sub $
      choice
        [ filePathSetting
            [ help "Path to database file",
              name "database"
            ],
          runIO $ do
            dataDir <- getXdgDir XdgData (Just [reldir|smos|])
            resolveFile dataDir "notify.sqlite3"
        ]
  setNotifySend <-
    sub $
      choice
        [ filePathSetting
            [ help "Path to notify-send executable",
              name "notify-send"
            ],
          runIO $ do
            me <- findExecutable [relfile|notify-send|]
            case me of
              Nothing -> die "could not find a notify-send executable."
              Just e -> pure e
        ]
  setLogLevel <- sub settingsParser
  pure Settings {..}
