{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.GitHub.OptParse
  ( module Smos.GitHub.OptParse,
    module Smos.GitHub.OptParse.Types,
  )
where

import Control.Monad
import Data.Version
import qualified Env
import Options.Applicative
import Options.Applicative.Help.Pretty as Doc
import Path
import Paths_smos_github
import Smos.Data
import Smos.GitHub.OptParse.Types
import Smos.Query.OptParse (getColourSettings)
import qualified Smos.Report.Config as Report
import qualified Smos.Report.OptParse as Report
import qualified System.Environment as System
import System.Exit

getInstructions :: IO Instructions
getInstructions = do
  (Arguments cmd flags) <- getArguments
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToInstructions cmd (Report.flagWithRestFlags flags) (Report.envWithRestEnv env) config

combineToInstructions :: Command -> Flags -> Environment -> Maybe Configuration -> IO Instructions
combineToInstructions cmd Flags {..} Environment {..} mc = do
  d <- case cmd of
    CommandList -> pure DispatchList
    CommandImport ImportFlags {..} -> do
      let importSetUrl = importFlagUrl
      let importSetForce = importFlagForce
      importDestinationFile <-
        forM importFlagFile $ \file ->
          case Path.Rel <$> parseRelFile file
            <|> Path.Abs <$> parseAbsFile file of
            Nothing -> die $ "Could not parse file path: " <> file
            Just someBase -> pure someBase
      importDestinationDirectory <-
        forM importFlagDirectory $ \file ->
          case Path.Rel <$> parseRelDir file
            <|> Path.Abs <$> parseAbsDir file of
            Nothing -> die $ "Could not parse directory path: " <> file
            Just someBase -> pure someBase
      let importSetDestination = ImportDestination {..}
      pure $ DispatchImport ImportSettings {..}
  setDirectorySettings <-
    Report.combineToDirectoryConfig
      Report.defaultDirectoryConfig
      flagDirectoryFlags
      envDirectoryEnvironment
      (confDirectoryConfiguration <$> mc)
  let setColourConfig = getColourSettings (mc >>= confColourConfiguration)
  let setGithubOauthToken = flagGithubOAuthToken <|> envGithubOAuthToken <|> cM githubConfOAuthToken
  pure (Instructions d Settings {..})
  where
    cM :: (GitHubConfiguration -> Maybe a) -> Maybe a
    cM func = mc >>= confGitHubConfiguration >>= func

getConfiguration :: Report.FlagsWithConfigFile Flags -> Report.EnvWithConfigFile Environment -> IO (Maybe Configuration)
getConfiguration = Report.getConfiguration

getEnvironment :: IO (Report.EnvWithConfigFile Environment)
getEnvironment = Env.parse (Env.header "Environment") prefixedEnvironmentParser

prefixedEnvironmentParser :: Env.Parser Env.Error (Report.EnvWithConfigFile Environment)
prefixedEnvironmentParser = Env.prefixed "SMOS_" environmentParser

environmentParser :: Env.Parser Env.Error (Report.EnvWithConfigFile Environment)
environmentParser =
  Report.envWithConfigFileParser $
    Environment
      <$> Report.directoryEnvironmentParser
      <*> optional (Env.var Env.str "GITHUB_OAUTH_TOKEN" (Env.help "Github Oauth Token"))

getArguments :: IO Arguments
getArguments = do
  args <- System.getArgs
  let result = runArgumentsParser args
  handleParseResult result

runArgumentsParser :: [String] -> ParserResult Arguments
runArgumentsParser = execParserPure prefs_ argumentsParser
  where
    prefs_ =
      defaultPrefs
        { prefShowHelpOnError = True,
          prefShowHelpOnEmpty = True
        }

argumentsParser :: ParserInfo Arguments
argumentsParser = info (helper <*> parseArguments) help_
  where
    help_ = fullDesc <> progDescDoc (Just description)
    description :: Doc
    description =
      Doc.vsep $
        map Doc.text $
          [ "",
            "Smos GitHub Tool version: " <> showVersion version,
            ""
          ]
            ++ readWriteDataVersionsHelpMessage

parseArguments :: Parser Arguments
parseArguments = Arguments <$> parseCommand <*> parseFlags

parseCommand :: Parser Command
parseCommand =
  hsubparser $
    mconcat
      [ command "list" parseCommandList,
        command "import" parseCommandImport
      ]

parseCommandList :: ParserInfo Command
parseCommandList = info parser modifier
  where
    modifier = fullDesc <> progDesc "List the relevant github issues"
    parser = pure CommandList

parseCommandImport :: ParserInfo Command
parseCommandImport = info parser modifier
  where
    modifier = fullDesc <> progDesc "Import a github issue as a smos project"
    parser =
      CommandImport
        <$> ( ImportFlags
                <$> strArgument
                  ( mconcat
                      [ help "The url to the issue to import",
                        metavar "URL"
                      ]
                  )
                <*> switch
                  ( mconcat
                      [ long "force",
                        help "Overwrite an existing file"
                      ]
                  )
                <*> optional
                  ( strOption
                      ( mconcat
                          [ help "File to put the resulting project in",
                            short 'f',
                            long "file",
                            metavar "FILEPATH",
                            completer $ bashCompleter "file"
                          ]
                      )
                  )
                <*> optional
                  ( strOption
                      ( mconcat
                          [ help "Directory to put the resulting project in",
                            short 'd',
                            long "directory",
                            metavar "FILEPATH",
                            completer $ bashCompleter "directory"
                          ]
                      )
                  )
            )

parseFlags :: Parser (Report.FlagsWithConfigFile Flags)
parseFlags =
  Report.parseFlagsWithConfigFile $
    Flags
      <$> Report.parseDirectoryFlags
      <*> optional
        ( strOption
            ( mconcat
                [ short 'g',
                  long "github-oauth-token",
                  metavar "OAUTH_TOKEN",
                  help "A github OAuth token"
                ]
            )
        )
