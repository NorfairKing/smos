module Smos.OptParse.Bare
  ( getPathArgument,
    editParser,
  )
where

import Import
import Options.Applicative
import System.Environment (getArgs)

getPathArgument :: IO (Path Abs File)
getPathArgument = do
  fp <- runArgumentsParser <$> getArgs >>= handleParseResult
  resolveFile' fp

runArgumentsParser :: [String] -> ParserResult FilePath
runArgumentsParser = execParserPure prefs_ argParser
  where
    prefs_ =
      defaultPrefs
        { prefShowHelpOnError = True,
          prefShowHelpOnEmpty = True
        }

argParser :: ParserInfo FilePath
argParser = info (helper <*> editParser) help_
  where
    help_ = fullDesc <> progDesc description
    description = "Smos editor"

editParser :: Parser FilePath
editParser =
  strArgument
    ( mconcat
        [ metavar "FILE",
          help "the file to edit",
          completer $ bashCompleter "file",
          value "/tmp/example.smos"
        ]
    )
