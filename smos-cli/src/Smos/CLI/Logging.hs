{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Smos.CLI.Logging where

import Autodocodec
import Control.Applicative
import Control.Arrow (left)
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Maybe
import qualified Data.Text as T
import qualified Env
import OptEnvConf
import Options.Applicative as OptParse
import Text.Read

runFilteredLogger :: (MonadIO m) => LogLevel -> LoggingT m a -> m a
runFilteredLogger logLevel =
  runStderrLoggingT
    . filterLogger (\_ ll -> ll >= logLevel)

combineLogLevelSettings :: Maybe LogLevel -> Maybe LogLevel -> Maybe LogLevel -> LogLevel
combineLogLevelSettings flagLogLevel envLogLevel confLogLevel =
  fromMaybe LevelInfo $ flagLogLevel <|> envLogLevel <|> confLogLevel

logLevelEnvParser :: String -> Either Env.Error LogLevel
logLevelEnvParser = left Env.UnreadError . parseLogLevel

parseLogLevelOption :: OptParse.Parser (Maybe LogLevel)
parseLogLevelOption =
  optional
    ( OptParse.option
        (OptParse.eitherReader parseLogLevel)
        ( mconcat
            [ OptParse.long "log-level",
              OptParse.help $
                unwords
                  [ "The log level to use, options:",
                    show $ map renderLogLevel logLevelOptions
                  ]
            ]
        )
    )

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
