{-# LANGUAGE OverloadedStrings #-}

module Smos.ASCIInema.OptParse
  ( module Smos.ASCIInema.OptParse,
    module Smos.ASCIInema.OptParse.Types,
  )
where

import qualified Env
import Options.Applicative
import Path.IO
import Smos.ASCIInema.OptParse.Types
import qualified System.Environment as System

getInstructions :: IO Instructions
getInstructions = do
  (Arguments cmd flags) <- getArguments
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToInstructions cmd flags env config

combineToInstructions :: Command -> Flags -> Environment -> Maybe Configuration -> IO Instructions
combineToInstructions (CommandRecord fp) Flags Environment _ = do
  d <- DispatchRecord <$> resolveFile' fp
  pure (Instructions d Settings)

getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration _ _ = pure Nothing

getEnvironment :: IO Environment
getEnvironment = Env.parse (Env.header "Environment") prefixedEnvironmentParser

prefixedEnvironmentParser :: Env.Parser Env.Error Environment
prefixedEnvironmentParser = Env.prefixed "SMOS_" environmentParser

environmentParser :: Env.Parser Env.Error Environment
environmentParser = pure Environment

getArguments :: IO Arguments
getArguments = do
  args <- System.getArgs
  let result = runArgumentsParser args
  handleParseResult result

runArgumentsParser :: [String] -> ParserResult Arguments
runArgumentsParser = execParserPure prefs_ argumentsParser
  where
    prefs_ =
      ParserPrefs
        { prefMultiSuffix = "",
          prefDisambiguate = True,
          prefShowHelpOnError = True,
          prefShowHelpOnEmpty = True,
          prefBacktrack = True,
          prefColumns = 80
        }

argumentsParser :: ParserInfo Arguments
argumentsParser = info (helper <*> parseArguments) help_
  where
    help_ = fullDesc <> progDesc description
    description = "smos-scheduler"

parseArguments :: Parser Arguments
parseArguments = Arguments <$> parseCommand <*> parseFlags

parseCommand :: Parser Command
parseCommand =
  hsubparser $
    mconcat
      [ command "record" parseCommandRecord
      ]

parseCommandRecord :: ParserInfo Command
parseCommandRecord = info parser modifier
  where
    modifier = fullDesc <> progDesc "Record an asciinema"
    parser =
      CommandRecord
        <$> strArgument
          ( mconcat
              [ help "The instructions file",
                metavar "FILEPATH"
              ]
          )

parseFlags :: Parser Flags
parseFlags = pure Flags
