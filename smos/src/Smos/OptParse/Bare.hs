module Smos.OptParse.Bare
  ( getPathArgument,
    resolveStartingPath,
    editParser,
  )
where

import Import
import Options.Applicative
import Smos.CLI.OptParse as CLI
import Smos.Types
import qualified System.Directory as FP
import System.Environment (getArgs)

getPathArgument :: IO (Maybe StartingPath)
getPathArgument = do
  fp <- getArgs >>= handleParseResult . runArgumentsParser
  curDir <- getCurrentDir
  mapM (resolveStartingPath curDir) fp

resolveStartingPath :: Path Abs Dir -> FilePath -> IO StartingPath
resolveStartingPath curDir fp = do
  dirExists <- FP.doesDirectoryExist fp
  if dirExists
    then StartingDir <$> resolveDir curDir fp
    else do
      p <- resolveFile curDir fp
      StartingFile <$> case fileExtension p of
        Nothing -> replaceExtension ".smos" p
        Just _ -> pure p

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
