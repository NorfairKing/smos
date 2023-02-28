{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Sizing.OptParse
  ( module Smos.Sizing.OptParse,
    module Smos.Sizing.OptParse.Types,
  )
where

import Data.Version
import qualified Env
import Options.Applicative
import Options.Applicative.Help.Pretty as Doc
import Path.IO
import Paths_smos_sizing
import Smos.Data
import qualified Smos.Report.OptParse as Report
import Smos.Sizing.OptParse.Types
import qualified System.Environment as System

getInstructions :: IO Instructions
getInstructions = do
  (Arguments cmd flags) <- getArguments
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToInstructions cmd (Report.flagWithRestFlags flags) (Report.envWithRestEnv env) config

combineToInstructions :: Command -> Flags -> Environment -> Maybe Configuration -> IO Instructions
combineToInstructions cmd Flags Environment _ = do
  d <- case cmd of
    CommandReport ReportFlags {..} -> do
      reportSettingPlanFile <- resolveFile' reportFlagPlanFile
      pure $ DispatchReport ReportSettings {..}
  pure (Instructions d Settings)

getConfiguration :: Report.FlagsWithConfigFile Flags -> Report.EnvWithConfigFile Environment -> IO (Maybe Configuration)
getConfiguration = Report.getConfiguration

getEnvironment :: IO (Report.EnvWithConfigFile Environment)
getEnvironment = Env.parse (Env.header "Environment") prefixedEnvironmentParser

prefixedEnvironmentParser :: Env.Parser Env.Error (Report.EnvWithConfigFile Environment)
prefixedEnvironmentParser = Env.prefixed "SMOS_" environmentParser

environmentParser :: Env.Parser Env.Error (Report.EnvWithConfigFile Environment)
environmentParser =
  Report.envWithConfigFileParser $
    pure Environment

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
            "Smos Sizing Tool version: " <> showVersion version,
            ""
          ]
            ++ readWriteDataVersionsHelpMessage

parseArguments :: Parser Arguments
parseArguments = Arguments <$> parseCommand <*> parseFlags

parseCommand :: Parser Command
parseCommand =
  hsubparser $
    mconcat
      [ command "report" parseCommandReport
      ]

parseCommandReport :: ParserInfo Command
parseCommandReport = info parser modifier
  where
    modifier = fullDesc <> progDesc "Report on sizing information"
    parser =
      CommandReport
        <$> ( ReportFlags
                <$> strArgument
                  ( mconcat
                      [ help "The smos file with the plan"
                      ]
                  )
            )

parseFlags :: Parser (Report.FlagsWithConfigFile Flags)
parseFlags =
  Report.parseFlagsWithConfigFile $
    pure Flags
