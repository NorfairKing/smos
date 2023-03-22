{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Web.Server.OptParse
  ( module Smos.Web.Server.OptParse,
    module Smos.Web.Server.OptParse.Types,
  )
where

import Autodocodec.Yaml
import Data.Maybe
import qualified Data.Text as T
import Data.Version
import qualified Env
import Options.Applicative
import Options.Applicative.Help.Pretty as Doc
import Path.IO
import Paths_smos_web_server
import Servant.Client
import Smos.CLI.Logging
import qualified Smos.CLI.OptParse as CLI
import Smos.Client
import Smos.Web.Server.OptParse.Types
import qualified System.Environment as System
import System.Exit

getSettings :: IO Settings
getSettings = do
  flags <- getFlags
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToSettings (CLI.flagWithRestFlags flags) (CLI.envWithRestEnv env) config

combineToSettings :: Flags -> Environment -> Maybe Configuration -> IO Settings
combineToSettings Flags {..} Environment {..} mConf = do
  let mc :: (Configuration -> Maybe a) -> Maybe a
      mc func = mConf >>= func
  let settingLogLevel = combineLogLevelSettings flagLogLevel envLogLevel (mc confLogLevel)
  let settingPort = fromMaybe 8000 $ flagPort <|> envPort <|> mc confPort
  settingAPIUrl <- case flagAPIUrl <|> envAPIUrl <|> mc confAPIUrl of
    Nothing -> die "No API configured."
    Just url -> parseBaseUrl url
  settingWebUrl <- case flagWebUrl <|> envWebUrl <|> mc confWebUrl of
    Nothing -> die "No web url configured."
    Just url -> parseBaseUrl url
  settingDocsUrl <- mapM parseBaseUrl $ flagDocsUrl <|> envDocsUrl <|> mc confDocsUrl
  settingDataDir <- case flagDataDir <|> envDataDir <|> mc confDataDir of
    Nothing -> getCurrentDir
    Just dd -> resolveDir' dd
  let settingGoogleAnalyticsTracking = T.pack <$> (flagGoogleAnalyticsTracking <|> envGoogleAnalyticsTracking <|> mc confGoogleAnalyticsTracking)
  let settingGoogleSearchConsoleVerification = T.pack <$> (flagGoogleSearchConsoleVerification <|> envGoogleSearchConsoleVerification <|> mc confGoogleSearchConsoleVerification)
  pure Settings {..}

getEnvironment :: IO (CLI.EnvWithConfigFile Environment)
getEnvironment = Env.parse (Env.header "Environment") prefixedEnvironmentParser

prefixedEnvironmentParser :: Env.Parser Env.Error (CLI.EnvWithConfigFile Environment)
prefixedEnvironmentParser =
  Env.prefixed
    "SMOS_WEB_SERVER_"
    environmentParser

environmentParser :: Env.Parser Env.Error (CLI.EnvWithConfigFile Environment)
environmentParser =
  CLI.envWithConfigFileParser $
    Environment
      <$> optional (Env.var logLevelEnvParser "LOG_LEVEL" (Env.help "The minimal severity of log messages"))
      <*> optional (Env.var Env.auto "PORT" (Env.help "The port to serve web requests on"))
      <*> optional (Env.var Env.str "DOCS_URL" (Env.help "The url to the docs site to refer to"))
      <*> optional (Env.var Env.str "API_URL" (Env.help "The url for the api to use"))
      <*> optional (Env.var Env.str "WEB_URL" (Env.help "The url that this web server is served from"))
      <*> optional (Env.var Env.str "DATA_DIR" (Env.help "The directory to store workflows during editing"))
      <*> optional (Env.var Env.str "GOOGLE_ANALYTICS_TRACKING" (Env.help "The Google analytics tracking code"))
      <*> optional (Env.var Env.str "GOOGLE_SEARCH_CONSOLE_VERIFICATION" (Env.help "The Google search console verification code"))

getConfiguration :: CLI.FlagsWithConfigFile b -> CLI.EnvWithConfigFile c -> IO (Maybe Configuration)
getConfiguration CLI.FlagsWithConfigFile {..} CLI.EnvWithConfigFile {..} =
  case flagWithConfigFile <|> envWithConfigFile of
    Nothing -> pure Nothing
    Just cf -> resolveFile' cf >>= readYamlConfigFile

getFlags :: IO (CLI.FlagsWithConfigFile Flags)
getFlags = do
  args <- System.getArgs
  let result = runFlagsParser args
  handleParseResult result

runFlagsParser :: [String] -> ParserResult (CLI.FlagsWithConfigFile Flags)
runFlagsParser = CLI.execOptionParserPure argParser

argParser :: ParserInfo (CLI.FlagsWithConfigFile Flags)
argParser = info (helper <*> parseArgs) help_
  where
    help_ = fullDesc <> progDescDoc (Just description)
    description :: Doc
    description =
      Doc.vsep $
        map Doc.text $
          [ "",
            "Smos Web Server version: " <> showVersion version,
            ""
          ]
            ++ clientVersionsHelpMessage

parseArgs :: Parser (CLI.FlagsWithConfigFile Flags)
parseArgs =
  CLI.parseFlagsWithConfigFile $
    Flags
      <$> parseLogLevelOption
      <*> optional
        ( option
            auto
            ( mconcat
                [ long "port",
                  metavar "PORT",
                  help "The port to serve web requests on"
                ]
            )
        )
      <*> optional
        ( strOption
            ( mconcat
                [ long "docs-url",
                  metavar "URL",
                  help "The url to the docs site to refer to"
                ]
            )
        )
      <*> optional
        ( strOption
            ( mconcat
                [ long "api-url",
                  metavar "URL",
                  help "The url for the api to use"
                ]
            )
        )
      <*> optional
        ( strOption
            ( mconcat
                [ long "web-url",
                  metavar "URL",
                  help "The url that this web server is served from"
                ]
            )
        )
      <*> optional
        ( strOption
            ( mconcat
                [ long "data-dir",
                  metavar "FILEPATH",
                  help "The directory to store workflows during editing"
                ]
            )
        )
      <*> optional
        ( strOption
            ( mconcat
                [ long "google-analytics-tracking",
                  metavar "CODE",
                  help "The Google analytics tracking code"
                ]
            )
        )
      <*> optional
        ( strOption
            ( mconcat
                [ long "google-search-console-verification",
                  metavar "CODE",
                  help "The Google search console verification code"
                ]
            )
        )
