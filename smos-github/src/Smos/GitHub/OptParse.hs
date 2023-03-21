{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.GitHub.OptParse
  ( module Smos.GitHub.OptParse,
    module Smos.GitHub.OptParse.Types,
  )
where

import Control.Monad
import qualified Data.ByteString as SB
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Version
import qualified Env
import Options.Applicative
import Options.Applicative.Help.Pretty as Doc
import Path
import Paths_smos_github
import Smos.CLI.Colour
import Smos.CLI.OptParse as CLI
import Smos.Data
import Smos.Directory.Config
import Smos.Directory.OptParse
import Smos.GitHub.OptParse.Types
import qualified System.Environment as System
import System.Exit

getInstructions :: IO Instructions
getInstructions = do
  (Arguments cmd flags) <- getArguments
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToInstructions cmd (flagWithRestFlags flags) (envWithRestEnv env) config

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
    combineToDirectorySettings
      defaultDirectorySettings
      flagDirectoryFlags
      envDirectoryEnvironment
      (confDirectorySettingsuration <$> mc)
  let setColourConfig = getColourSettings (mc >>= confColourConfiguration)
  let mTok mToken mTokenFile = case mToken of
        Just token -> pure $ Just token
        Nothing -> case mTokenFile of
          Nothing -> pure Nothing
          Just tokenFile -> Just . T.strip . TE.decodeUtf8 <$> SB.readFile tokenFile
  flagMTok <- mTok flagGitHubOAuthToken flagGitHubOAuthTokenFile
  envMTok <- mTok envGitHubOAuthToken envGitHubOAuthTokenFile
  confMTok <- mTok (cM githubConfOAuthToken) (cM githubConfOAuthTokenFile)
  let setGitHubOauthToken = flagMTok <|> envMTok <|> confMTok
  pure (Instructions d Settings {..})
  where
    cM :: (GitHubConfiguration -> Maybe a) -> Maybe a
    cM func = mc >>= confGitHubConfiguration >>= func

getEnvironment :: IO (EnvWithConfigFile Environment)
getEnvironment = Env.parse (Env.header "Environment") prefixedEnvironmentParser

prefixedEnvironmentParser :: Env.Parser Env.Error (EnvWithConfigFile Environment)
prefixedEnvironmentParser = Env.prefixed "SMOS_" environmentParser

environmentParser :: Env.Parser Env.Error (EnvWithConfigFile Environment)
environmentParser =
  envWithConfigFileParser $
    Environment
      <$> directoryEnvironmentParser
      <*> optional (Env.var Env.str "GITHUB_OAUTH_TOKEN" (Env.help "GitHub Oauth Token"))
      <*> optional (Env.var Env.str "GITHUB_OAUTH_TOKEN_FILE" (Env.help "GitHub Oauth Token File"))

getArguments :: IO Arguments
getArguments = do
  args <- System.getArgs
  let result = runArgumentsParser args
  handleParseResult result

runArgumentsParser :: [String] -> ParserResult Arguments
runArgumentsParser = CLI.execOptionParserPure argumentsParser

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

parseFlags :: Parser (FlagsWithConfigFile Flags)
parseFlags =
  parseFlagsWithConfigFile $
    Flags
      <$> parseDirectoryFlags
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
      <*> optional
        ( strOption
            ( mconcat
                [ long "github-oauth-token-file",
                  metavar "OAUTH_TOKEN_FILE",
                  help "A github OAuth token file"
                ]
            )
        )
