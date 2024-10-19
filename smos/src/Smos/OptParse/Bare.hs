module Smos.OptParse.Bare
  ( getPathArgument,
    editParser,
  )
where

import Options.Applicative
import Path.IO
import Smos.CLI.OptParse as CLI
import Smos.Types
import System.Environment (getArgs)

getPathArgument :: IO (Maybe StartingPath)
getPathArgument = do
  fp <- getArgs >>= handleParseResult . runArgumentsParser
  curDir <- getCurrentDir
  mapM (resolveStartingPath curDir) fp

runArgumentsParser :: [String] -> ParserResult (Maybe FilePath)
runArgumentsParser = CLI.execOptionParserPure argParser

argParser :: ParserInfo (Maybe FilePath)
argParser = info (helper <*> editParser) help_
  where
    help_ = fullDesc <> progDesc description
    description = "Smos editor"

editParser :: Parser (Maybe FilePath)
editParser =
  optional $
    strArgument
      ( mconcat
          [ metavar "FILE",
            help "the file to edit",
            completer $ bashCompleter "file"
          ]
      )
