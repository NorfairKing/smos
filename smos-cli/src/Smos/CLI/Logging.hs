{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Smos.CLI.Logging where

import Autodocodec
import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.Text as T
import OptEnvConf
import Text.Read

runFilteredLogger :: (MonadIO m) => LogLevel -> LoggingT m a -> m a
runFilteredLogger logLevel =
  runStderrLoggingT
    . filterLogger (\_ ll -> ll >= logLevel)

instance HasCodec LogLevel where
  codec =
    named "LogLevel" $
      bimapCodec parseLogLevel renderLogLevel codec
        <??> [ "The log level to use, options:",
               T.pack $ show $ map renderLogLevel logLevelOptions
             ]

instance OptEnvConf.HasParser LogLevel where
  settingsParser =
    setting $
      concat
        [ [ OptEnvConf.help "Minimal severity of log messages",
            name "log-level",
            reader $ OptEnvConf.eitherReader parseLogLevel,
            OptEnvConf.metavar "LOG_LEVEL",
            valueWithShown LevelInfo (renderLogLevel LevelInfo)
          ],
          map (example . renderLogLevel) logLevelOptions
        ]

logLevelOptions :: [LogLevel]
logLevelOptions = [LevelDebug, LevelInfo, LevelWarn, LevelError]

parseLogLevel :: String -> Either String LogLevel
parseLogLevel s = case readMaybe $ "Level" <> s of
  Nothing -> Left $ unwords ["Unknown log level: " <> show s]
  Just ll -> Right ll

renderLogLevel :: LogLevel -> String
renderLogLevel = drop 5 . show
