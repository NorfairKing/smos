{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Convert.Org.OptParse
  ( getSettings,
  )
where

import Options.Applicative
import Path.IO
import Smos.Convert.Org.OptParse.Types
import Smos.Version
import System.Environment

getSettings :: IO Settings
getSettings = do
  flags <- getFlags
  config <- getConfig flags
  deriveSettings flags config

getConfig :: Flags -> IO Configuration
getConfig Flags {..} = pure Configuration

deriveSettings :: Flags -> Configuration -> IO Settings
deriveSettings Flags {..} Configuration = do
  setFromFile <- resolveFile' flagFromFile
  setToFile <- mapM resolveFile' flagToFile
  pure Settings {..}

getFlags :: IO Flags
getFlags = do
  args <- getArgs
  let result = runArgumentsParser args
  handleParseResult result

runArgumentsParser :: [String] -> ParserResult Flags
runArgumentsParser = execParserPure prefs_ flagsParser
  where
    prefs_ =
      defaultPrefs
        { prefShowHelpOnError = True,
          prefShowHelpOnEmpty = True
        }

flagsParser :: ParserInfo Flags
flagsParser = info (helper <*> parseFlags) help_
  where
    help_ = fullDesc <> progDesc description
    description = "Smos Org-mode Conversion Tool: " <> smosVersion

parseFlags :: Parser Flags
parseFlags =
  Flags <$> strArgument (mconcat [help "The file to convert", metavar "FILEPATH"])
    <*> option
      (Just <$> str)
      (mconcat [long "to", help "The output file", value Nothing, metavar "FILEPATH"])
