{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Docs.Site.OptParse
  ( module Smos.Docs.Site.OptParse,
    module Smos.Docs.Site.OptParse.Types,
  )
where

import Autodocodec.Yaml (readYamlConfigFile)
import Data.Maybe
import qualified Data.Text as T
import Data.Version
import qualified Env
import Options.Applicative
import Path.IO
import Paths_smos_docs_site
import Smos.Docs.Site.OptParse.Types
import qualified System.Environment as System

getInstructions :: IO Instructions
getInstructions = do
  args@(Arguments _ flags) <- getArguments
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToInstructions args env config

combineToInstructions :: Arguments -> Environment -> Maybe Configuration -> IO Instructions
combineToInstructions (Arguments (CommandServe ServeFlags {..}) _) Environment {..} mConf = do
  let mc :: (Configuration -> Maybe a) -> Maybe a
      mc func = mConf >>= func
  let serveSetPort = fromMaybe 8000 $ serveFlagPort <|> envPort <|> mc confPort
  let serveSetAPIServerUrl = T.pack <$> (serveFlagAPIServerUrl <|> envAPIServerUrl <|> mc confAPIServerUrl)
  let serveSetWebServerUrl = T.pack <$> (serveFlagWebServerUrl <|> envWebServerUrl <|> mc confWebServerUrl)
  let serveSetGoogleAnalyticsTracking = T.pack <$> (serveFlagGoogleAnalyticsTracking <|> envGoogleAnalyticsTracking <|> mc confGoogleAnalyticsTracking)
  let serveSetGoogleSearchConsoleVerification = T.pack <$> (serveFlagGoogleSearchConsoleVerification <|> envGoogleSearchConsoleVerification <|> mc confGoogleSearchConsoleVerification)
  pure (Instructions (DispatchServe ServeSettings {..}) Settings)

getEnvironment :: IO Environment
getEnvironment = Env.parse (Env.header "Environment") prefixedEnvironmentParser

prefixedEnvironmentParser :: Env.Parser Env.Error Environment
prefixedEnvironmentParser =
  Env.prefixed
    "SMOS_DOCS_SITE_"
    environmentParser

environmentParser :: Env.Parser Env.Error Environment
environmentParser =
  Environment
    <$> optional (Env.var Env.str "CONFIG_FILE" (Env.help "The config file"))
    <*> optional (Env.var Env.auto "PORT" (Env.help "The port to serve web requests on"))
    <*> optional (Env.var Env.str "API_URL" (Env.help "The url for the api server to refer to"))
    <*> optional (Env.var Env.str "WEB_URL" (Env.help "The url for the web server to refer to"))
    <*> optional (Env.var Env.str "GOOGLE_ANALYTICS_TRACKING" (Env.help "The Google analytics tracking code"))
    <*> optional (Env.var Env.str "GOOGLE_SEARCH_CONSOLE_VERIFICATION" (Env.help "The Google search console verification code"))

getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} =
  case flagConfigFile <|> envConfigFile of
    Nothing -> pure Nothing
    Just cf -> resolveFile' cf >>= readYamlConfigFile

getArguments :: IO Arguments
getArguments = do
  args <- System.getArgs
  let result = runArgumentsParser args
  handleParseResult result

runArgumentsParser :: [String] -> ParserResult Arguments
runArgumentsParser = execParserPure prefs_ argParser
  where
    prefs_ =
      defaultPrefs
        { prefShowHelpOnError = True,
          prefShowHelpOnEmpty = True
        }

argParser :: ParserInfo Arguments
argParser = info (helper <*> parseArgs) help_
  where
    help_ = fullDesc <> progDesc description
    description = "Smos Web Server version " <> showVersion version

parseArgs :: Parser Arguments
parseArgs = Arguments <$> parseCommand <*> parseFlags

parseCommand :: Parser Command
parseCommand = hsubparser $ mconcat [command "serve" parseCommandServe]

parseCommandServe :: ParserInfo Command
parseCommandServe = info parser modifier
  where
    modifier = fullDesc <> progDesc "Serve as the web server"
    parser =
      CommandServe
        <$> ( ServeFlags
                <$> optional
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
                          [ long "api-url",
                            metavar "URL",
                            help "The url to the api server to refer to"
                          ]
                      )
                  )
                <*> optional
                  ( strOption
                      ( mconcat
                          [ long "web-url",
                            metavar "URL",
                            help "The url to the web server to refer to"
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
            )

parseFlags :: Parser Flags
parseFlags =
  Flags
    <$> optional
      ( strOption
          ( mconcat
              [ long "config-file",
                metavar "FILEPATH",
                help "The config file"
              ]
          )
      )
