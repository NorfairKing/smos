{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Convert.Org.OptParse
  ( getSettings,
  )
where

import Data.Version
import Options.Applicative
import Options.Applicative.Help.Pretty as Doc
import Path.IO
import Paths_smos_convert_org
import Smos.Convert.Org.OptParse.Types
import Smos.Data
import System.Environment

getSettings :: IO Settings
getSettings = do
  flags <- getFlags
  deriveSettings flags

deriveSettings :: Flags -> IO Settings
deriveSettings Flags {..} = do
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
    help_ = fullDesc <> progDescDoc (Just description)
    description :: Doc
    description =
      Doc.vsep $
        map Doc.text $
          [ "",
            "Smos Org-mode Conversion Tool version: " <> showVersion version,
            ""
          ]
            ++ writeDataVersionsHelpMessage

parseFlags :: Parser Flags
parseFlags =
  Flags <$> strArgument (mconcat [help "The file to convert", metavar "FILEPATH"])
    <*> option
      (Just <$> str)
      (mconcat [long "to", help "The output file", value Nothing, metavar "FILEPATH"])
