{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Web.Server.OptParse
  ( module Smos.Web.Server.OptParse,
    module Smos.Web.Server.OptParse.Types,
  )
where

import Control.Monad
import Control.Monad.Logger
import Data.Maybe
import qualified Env
import Options.Applicative
import Path.IO
import Servant.Client
import qualified Smos.Server.OptParse as API
import Smos.Web.Server.OptParse.Types
import qualified System.Environment as System
import System.Exit
import YamlParse.Applicative (readConfigFile)

getInstructions :: IO Instructions
getInstructions = do
  args@(Arguments _ flags) <- getArguments
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToInstructions args env config

combineToInstructions :: Arguments -> Environment -> Maybe Configuration -> IO Instructions
combineToInstructions (Arguments (CommandServe ServeFlags {..}) Flags {..}) Environment {..} mConf = do
  API.Instructions (API.DispatchServe serveSetAPISettings) API.Settings <-
    API.combineToInstructions
      (API.Arguments (API.CommandServe serveFlagAPIFlags) flagAPIFlags)
      envAPIEnv
      (confAPIConfiguration <$> mConf)
  let WebServerEnvironment {..} = envWebServerEnv
  let mc :: (Configuration -> Maybe a) -> Maybe a
      mc func = mConf >>= func
  let serveSetLogLevel = fromMaybe LevelInfo $ serveFlagLogLevel <|> envLogLevel <|> mc confLogLevel
  let serveSetPort = fromMaybe 8000 $ serveFlagPort <|> envPort <|> mc confPort
  serveSetDocsUrl <- mapM parseBaseUrl $ serveFlagDocsUrl <|> envDocsUrl <|> mc confDocsUrl
  when (serveSetPort == API.serveSetPort serveSetAPISettings) $ die $ "The port for the api server and the web server are the same: " <> show serveSetPort
  pure (Instructions (DispatchServe ServeSettings {..}) Settings)

getEnvironment :: IO Environment
getEnvironment = Env.parse (Env.header "Environment") environmentParser

environmentParser :: Env.Parser Env.Error Environment
environmentParser =
  Environment
    <$> API.environmentParser
    <*> Env.prefixed "SMOS_WEB_SERVER_" webServerEnvironmentParser

webServerEnvironmentParser :: Env.Parser Env.Error WebServerEnvironment
webServerEnvironmentParser =
  WebServerEnvironment
    <$> Env.var (fmap Just . (maybe (Left $ Env.UnreadError "Unknown log level") Right . API.parseLogLevel)) "LOG_LEVEL" (mE <> Env.help "The minimal severity of log messages")
    <*> Env.var (fmap Just . Env.auto) "PORT" (mE <> Env.help "The port to serve web requests on")
    <*> Env.var (fmap Just . Env.str) "DOCS_URL" (mE <> Env.help "The url to the docs site to refer to")
  where
    mE = Env.def Nothing <> Env.keep

getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} =
  case API.flagConfigFile flagAPIFlags <|> API.envConfigFile envAPIEnv of
    Nothing -> pure Nothing
    Just cf -> resolveFile' cf >>= readConfigFile

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
    description = "smos-web-server"

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
                <$> API.parseServeFlags
                <*> option
                  (Just <$> maybeReader API.parseLogLevel)
                  ( mconcat
                      [ long "web-log-level",
                        help $
                          unwords
                            [ "The log level to use, options:",
                              show $ map API.renderLogLevel [LevelDebug, LevelInfo, LevelWarn, LevelError]
                            ],
                        value Nothing
                      ]
                  )
                <*> option
                  (Just <$> auto)
                  ( mconcat
                      [ long "web-port",
                        metavar "PORT",
                        help "The port to serve web requests on",
                        value Nothing
                      ]
                  )
                <*> option
                  (Just <$> str)
                  ( mconcat
                      [ long "docs-url",
                        metavar "URL",
                        help "The url to the docs site to refer to",
                        value Nothing
                      ]
                  )
            )

parseFlags :: Parser Flags
parseFlags =
  Flags <$> API.parseFlags
