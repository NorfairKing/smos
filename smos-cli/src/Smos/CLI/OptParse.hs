{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.CLI.OptParse
  ( execOptionParserPure,
    parseFlagsWithConfigFile,
    FlagsWithConfigFile (..),
    envWithConfigFileParser,
    EnvWithConfigFile (..),
    getConfiguration,
  )
where

import Autodocodec
import Autodocodec.Yaml
import qualified Env
import Options.Applicative
import Path
import Path.IO

execOptionParserPure :: ParserInfo a -> [String] -> ParserResult a
execOptionParserPure = execParserPure smosPrefs
  where
    smosPrefs :: ParserPrefs
    smosPrefs =
      prefs $
        mconcat
          [ showHelpOnError,
            showHelpOnEmpty,
            subparserInline,
            helpShowGlobals
          ]

data FlagsWithConfigFile a = FlagsWithConfigFile
  { flagWithConfigFile :: Maybe FilePath,
    flagWithRestFlags :: a
  }

parseFlagsWithConfigFile :: Parser a -> Parser (FlagsWithConfigFile a)
parseFlagsWithConfigFile p =
  FlagsWithConfigFile <$> parseConfigFileFlag <*> p

parseConfigFileFlag :: Parser (Maybe FilePath)
parseConfigFileFlag =
  optional
    ( strOption
        ( mconcat
            [ metavar "FILE_PATH",
              help "The config file to use",
              long "config-file",
              completer $ bashCompleter "file"
            ]
        )
    )

data EnvWithConfigFile a = EnvWithConfigFile
  { envWithConfigFile :: Maybe FilePath,
    envWithRestEnv :: a
  }

envWithConfigFileParser :: Env.Parser Env.Error a -> Env.Parser Env.Error (EnvWithConfigFile a)
envWithConfigFileParser p =
  EnvWithConfigFile
    <$> optional (Env.var Env.str "CONFIG_FILE" (Env.help "Workflow directory"))
    <*> p

getConfiguration :: (HasCodec a) => FlagsWithConfigFile b -> EnvWithConfigFile c -> IO (Maybe a)
getConfiguration FlagsWithConfigFile {..} EnvWithConfigFile {..} = do
  case flagWithConfigFile <|> envWithConfigFile of
    Just sf -> resolveFile' sf >>= readYamlConfigFile
    Nothing -> defaultConfigFiles >>= readFirstYamlConfigFile

-- | Smos Config files
--
-- Tried in order:
--
-- * ~/.config/smos/config.yaml
-- * ~/.smos/config.yaml
-- * ~/.smos.yaml
defaultConfigFiles :: IO [Path Abs File]
defaultConfigFiles = do
  home <- getHomeDir
  homeConfigDir <- resolveDir home ".smos"
  xdgConfigDir <- getXdgDir XdgConfig (Just [reldir|smos|])
  configInConfigDir <- resolveFile xdgConfigDir "config.yaml"
  configInHomeSmosDir <- resolveFile homeConfigDir "config.yaml"
  configInHomeDir <- resolveFile home "smos.yaml"
  pure [configInConfigDir, configInHomeSmosDir, configInHomeDir]
