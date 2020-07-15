{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.ASCIInema.OptParse
  ( module Smos.ASCIInema.OptParse,
    module Smos.ASCIInema.OptParse.Types,
  )
where

import Data.Maybe
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
combineToInstructions (CommandRecord RecordFlags {..}) Flags Environment {..} _ = do
  let recordSetWait = fromMaybe 1 recordFlagWait
  recordSetSpecFile <- resolveFile' recordFlagSpecFile
  recordSetOutputFile <- resolveFile' recordFlagOutputFile
  recordSetAsciinemaConfigDir <- mapM resolveDir' envAsciinemaConfigDir
  let d = DispatchRecord RecordSettings {..}
  pure (Instructions d Settings)

getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration _ _ = pure Nothing

getEnvironment :: IO Environment
getEnvironment = Env.parse (Env.header "Environment") environmentParser

environmentParser :: Env.Parser Env.Error Environment
environmentParser = Environment <$> optional (Env.var Env.str "ASCIINEMA_CONFIG_HOME" (Env.help "The directory for asciinema config files"))

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
        <$> ( RecordFlags
                <$> strArgument
                  ( mconcat
                      [ help "The instructions file",
                        metavar "SPEC_FILE"
                      ]
                  )
                <*> strArgument
                  ( mconcat
                      [ help "The output file",
                        metavar "OUTPUT_FILE"
                      ]
                  )
                <*> parseWaitFlag
            )

parseWaitFlag :: Parser (Maybe Double)
parseWaitFlag = optional $ option auto $ mconcat [long "wait", help "The wait-time multiplier", metavar "DOUBLE"]

parseFlags :: Parser Flags
parseFlags = pure Flags
