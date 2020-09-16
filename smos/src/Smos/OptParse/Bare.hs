module Smos.OptParse.Bare
  ( getPathArgument,
    resolveStartingPath,
    editParser,
  )
where

import Import
import Options.Applicative
import Smos.Types
import qualified System.Directory as FP
import System.Environment (getArgs)

getPathArgument :: IO (Maybe StartingPath)
getPathArgument = do
  fp <- runArgumentsParser <$> getArgs >>= handleParseResult
  mapM resolveStartingPath fp

resolveStartingPath :: FilePath -> IO StartingPath
resolveStartingPath fp = do
  dirExists <- FP.doesDirectoryExist fp
  if dirExists
    then StartingDir <$> resolveDir' fp
    else do
      p <- resolveFile' fp
      StartingFile <$> case fileExtension p of
        Nothing -> replaceExtension ".smos" p
        Just _ -> pure p

runArgumentsParser :: [String] -> ParserResult (Maybe FilePath)
runArgumentsParser = execParserPure prefs_ argParser
  where
    prefs_ =
      defaultPrefs
        { prefShowHelpOnError = True,
          prefShowHelpOnEmpty = True
        }

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
